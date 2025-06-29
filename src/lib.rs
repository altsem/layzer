//! ```txt
//! Layzer - A Lightweight Terminal Layout Library
//!
//! Layzer is a zero-heap-allocation layout library for
//! building nested layouts with discrete positioning
//! and sizing.
//! It's designed for terminal UIs, text-based interfaces,
//! and any scenario where you need to compute positions and
//! sizes of rectangular elements.
//!
//! - Zero heap allocations -           - Hierarchical layouts -
//! All layout computation is done      Support for nesting layouts
//! without dynamic memory allocation.  and arranging children within them.
//!
//! - Flexible sizing -               - Axis-oriented layouts -
//! Auto-sizing based on content or   Arrange  children  horizontally
//! fixed dimensions.                 children
//!                                   vertically
//! ```
//! ## Quick Start
//!
//! ```rust
//! use layzer::{Layout, LayoutMeta, Axis};
//!
//! // Create a text element
//! let text_layout = Layout::text("Hello, World!");
//!
//! // Create a container with children
//! let mut container = Layout {
//!     meta: LayoutMeta::default().vertical().gap(1),
//!     children: &mut [
//!         Layout::text("First line"),
//!         Layout::text("Second line"),
//!     ],
//! };
//!
//! // Compute layout within 80x24 bounds
//! container.compute([80, 24]);
//!
//! // Iterate over computed positions and data with a stack-size of 8
//! for (position, text) in container.iter::<8>() {
//!     println!("Text '{}' at position {:?}", text, position);
//! }
//! ```
//!
//! ## Layout Structure
//!
//! Layouts form a tree structure where each node can contain data and/or children.
//! The layout engine computes positions recursively, respecting the orientation
//! and sizing constraints of each node.
//!

use arrayvec::ArrayVec;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Axis {
    #[default]
    X = 0,
    Y = 1,
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Vec2<T>(T, T);

impl<T: std::fmt::Debug> std::fmt::Debug for Vec2<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Vec2({:?}, {:?})", self.0, self.1)
    }
}

impl<T> std::ops::Index<Axis> for Vec2<T> {
    type Output = T;

    fn index(&self, index: Axis) -> &Self::Output {
        match index {
            Axis::X => &self.0,
            Axis::Y => &self.1,
        }
    }
}

impl<T> std::ops::IndexMut<Axis> for Vec2<T> {
    fn index_mut(&mut self, index: Axis) -> &mut Self::Output {
        match index {
            Axis::X => &mut self.0,
            Axis::Y => &mut self.1,
        }
    }
}

#[derive(Debug)]
pub struct Layout<'a, T> {
    pub meta: LayoutMeta<T>,
    pub children: &'a mut [Layout<'a, T>],
}

#[derive(Debug)]
pub struct LayoutMeta<T> {
    data: Option<T>,
    orientation: Axis,
    sizing: Vec2<Sizing>,
    gap: usize,
    _computed_position: Vec2<usize>,
    _computed_size: Vec2<usize>,
}

impl<T> Default for LayoutMeta<T> {
    fn default() -> Self {
        Self {
            data: None,
            orientation: Axis::X,
            sizing: Vec2::default(),
            gap: 0,
            _computed_position: Vec2::default(),
            _computed_size: Vec2::default(),
        }
    }
}

impl<T> LayoutMeta<T> {
    pub fn data(self, data: T) -> Self {
        Self {
            data: Some(data),
            ..self
        }
    }

    pub fn vertical(self) -> Self {
        Self {
            orientation: Axis::Y,
            ..self
        }
    }

    pub fn sizing(self, x_sizing: Sizing, y_sizing: Sizing) -> Self {
        Self {
            sizing: Vec2(x_sizing, y_sizing),
            ..self
        }
    }

    pub fn gap(self, gap: usize) -> Self {
        Self { gap, ..self }
    }
}

#[derive(Debug, Default, Copy, Clone)]
pub enum Sizing {
    #[default]
    Auto,
    Fixed(usize),
}

pub struct LayoutIterator<'a, T, const STACK_SIZE: usize> {
    stack_buffer: ArrayVec<&'a Layout<'a, T>, STACK_SIZE>,
}

impl<'a, T, const STACK: usize> Iterator for LayoutIterator<'a, T, STACK> {
    type Item = &'a Layout<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.stack_buffer.pop().inspect(|layout| {
            for child in layout.children.iter() {
                self.stack_buffer.try_push(child).expect("Layout::iter");
            }
        })
    }
}

impl<'a, T: AsRef<str>> Layout<'a, T> {
    /// # Arguments
    /// * `data` - The text data to be displayed
    ///
    /// # Example
    /// ```
    /// use layzer::Layout;
    ///
    /// let text_layout = Layout::text("Hello, World!");
    /// // This creates a layout with dimensions 13x1 (13 characters wide, 1 line tall)
    /// ```
    pub fn text(text: T) -> Self {
        let len = text.as_ref().graphemes(true).count();

        Self {
            meta: LayoutMeta::default()
                .data(text)
                .sizing(Sizing::Fixed(len), Sizing::Fixed(1)),
            children: &mut [],
        }
    }

    /// Computes the layout of the current node and its children.
    ///
    /// This function recursively updates the inner state of the [Layout].
    pub fn compute(&mut self, bounds: [usize; 2]) {
        self.compute_axis(Axis::X, bounds[0]);
        self.compute_axis(Axis::Y, bounds[1]);
    }

    /// Returns an iterator over all layout elements and their computed positions.
    ///
    /// This function yields tuples of `(position, data)` for every layout node that contains data,
    /// traversing the [Layout] tree in depth-first order.
    ///
    /// Be sure to call [Layout::compute()] before iterating.
    ///
    /// # Stack size
    /// `STACK` - Specifies the size of the stack-allocated stack buffer.
    pub fn iter<const STACK_SIZE: usize>(&'a self) -> impl Iterator<Item = ([usize; 2], &'a T)> {
        let mut stack = ArrayVec::new();
        stack.push(self);

        LayoutIterator::<T, STACK_SIZE> {
            stack_buffer: stack,
        }
        .filter_map(|layout| {
            layout.meta.data.as_ref().map(|data| {
                (
                    [
                        layout.meta._computed_position.0,
                        layout.meta._computed_position.1,
                    ],
                    data,
                )
            })
        })
    }

    fn compute_axis(&mut self, axis: Axis, max_size: usize) -> usize {
        let computed_size = match self.meta.sizing[axis] {
            Sizing::Auto => self.compute_children(axis, max_size),
            Sizing::Fixed(fixed_size) => {
                let size = fixed_size.min(max_size);
                self.compute_children(axis, size);
                size
            }
        };

        self.meta._computed_size[axis] = computed_size;
        computed_size
    }

    fn compute_children(&mut self, axis: Axis, max_size: usize) -> usize {
        if self.meta.orientation == axis {
            self.children.iter_mut().fold(0, |acc, child| {
                let acc = if acc > 0 { acc + self.meta.gap } else { acc };

                child.meta._computed_position[axis] = self.meta._computed_position[axis] + acc;
                acc + child.compute_axis(axis, max_size)
            })
        } else {
            self.children
                .iter_mut()
                .map(|child| {
                    child.meta._computed_position[axis] = self.meta._computed_position[axis];
                    child.compute_axis(axis, max_size)
                })
                .max()
                .unwrap_or(0)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestBuffer<'a> {
        rows: [[&'a str; 80]; 40],
    }

    impl<'a> TestBuffer<'a> {
        fn new() -> Self {
            Self {
                rows: [[" "; 80]; 40],
            }
        }
    }

    impl<'a> FromIterator<([usize; 2], &'a &'a str)> for TestBuffer<'a> {
        fn from_iter<T: IntoIterator<Item = ([usize; 2], &'a &'a str)>>(iter: T) -> Self {
            let mut buffer = Self::new();

            for (position, text) in iter {
                for (i, g) in text.grapheme_indices(true) {
                    buffer.rows[position[1]][position[0] + i] = g;
                }
            }

            buffer
        }
    }

    impl<'a> std::fmt::Display for TestBuffer<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for row in self.rows {
                for char in row {
                    write!(f, "{}", char)?;
                }
                writeln!(f)?;
            }
            Ok(())
        }
    }

    #[test]
    fn two_columns() {
        let mut layout = Layout {
            meta: LayoutMeta::default()
                .data("________________________________________")
                .gap(2),
            children: &mut [
                Layout {
                    meta: LayoutMeta::default().vertical(),
                    children: &mut [
                        Layout {
                            meta: LayoutMeta::default().gap(1),
                            children: &mut [Layout::text("column"), Layout::text("one")],
                        },
                        Layout::text("column 1"),
                    ],
                },
                Layout {
                    meta: LayoutMeta::default().vertical(),
                    children: &mut [Layout::text("column 2"), Layout::text("column 2")],
                },
            ],
        };

        // FIXME Restricting bounds: `layout.compute([1. 1])` - doesn't have the desired effect
        layout.compute([40, 20]);
        insta::assert_snapshot!(layout.iter::<8>().collect::<TestBuffer>());
    }

    #[test]
    fn docs() {
        let mut layout = Layout {
            meta: LayoutMeta::default().vertical().gap(1),
            children: &mut [
                Layout::text("Layzer - A Lightweight Terminal Layout Library"),
                Layout {
                    meta: LayoutMeta::default().vertical(),
                    children: &mut [
                        Layout::text("Layzer is a zero-heap-allocation layout library for"),
                        Layout::text("building nested layouts with discrete positioning"),
                        Layout::text("and sizing."),
                        Layout::text("It's designed for terminal UIs, text-based interfaces,"),
                        Layout::text("and any scenario where you need to compute positions and"),
                        Layout::text("sizes of rectangular elements."),
                    ],
                },
                Layout {
                    meta: LayoutMeta::default().gap(2),
                    children: &mut [
                        Layout {
                            meta: LayoutMeta::default().vertical(),
                            children: &mut [
                                Layout::text("- Zero heap allocations -"),
                                Layout::text("All layout computation is done"),
                                Layout::text("without dynamic memory allocation."),
                            ],
                        },
                        Layout {
                            meta: LayoutMeta::default().vertical(),
                            children: &mut [
                                Layout::text("- Hierarchical layouts -"),
                                Layout::text("Support for nesting layouts"),
                                Layout::text("and arranging children within them."),
                            ],
                        },
                    ],
                },
                Layout {
                    meta: LayoutMeta::default().gap(2),
                    children: &mut [
                        Layout {
                            meta: LayoutMeta::default().vertical(),
                            children: &mut [
                                Layout::text("- Flexible sizing -"),
                                Layout::text("Auto-sizing based on content or "),
                                Layout::text("fixed dimensions."),
                            ],
                        },
                        Layout {
                            meta: LayoutMeta::default().vertical(),
                            children: &mut [
                                Layout::text("- Axis-oriented layouts -"),
                                Layout {
                                    meta: LayoutMeta::default().gap(2),
                                    children: &mut [
                                        Layout::text("Arrange"),
                                        Layout::text("children"),
                                        Layout::text("horizontally"),
                                    ],
                                },
                                Layout {
                                    meta: LayoutMeta::default().vertical(),
                                    children: &mut [
                                        Layout::text("children"),
                                        Layout::text("vertically"),
                                    ],
                                },
                            ],
                        },
                    ],
                },
            ],
        };

        layout.compute([100, 100]);
        insta::assert_snapshot!(layout.iter::<16>().collect::<TestBuffer>());
    }
}
