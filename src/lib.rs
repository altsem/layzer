//! ## A Lightweight Terminal Layout Library
//!
//! **Work-in-progress, unfinished and broken, be warned**
//!
//! ```txt
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
//! use layzer::{Layout, LayoutMeta};
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
enum Orientaiton {
    #[default]
    Horizontal,
    Vertical,
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
struct Vec2<T>(T, T);

impl<T> Vec2<T> {
    fn flip(self) -> Self {
        Self(self.1, self.0)
    }
}

impl<T> From<[T; 2]> for Vec2<T> {
    fn from([x, y]: [T; 2]) -> Self {
        Self(x, y)
    }
}

impl<T> From<Vec2<T>> for [T; 2] {
    fn from(val: Vec2<T>) -> Self {
        [val.0, val.1]
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Vec2<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Vec2({:?}, {:?})", self.0, self.1)
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
    orientation: Orientaiton,
    gap: usize,
    computed_position: Vec2<usize>,
    computed_size: Vec2<usize>,
    out_of_bounds: bool,
}

impl<T> Default for LayoutMeta<T> {
    fn default() -> Self {
        Self {
            data: None,
            orientation: Orientaiton::Horizontal,
            gap: 0,
            computed_position: Vec2::default(),
            computed_size: Vec2::default(),
            out_of_bounds: false,
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
            orientation: Orientaiton::Vertical,
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

/// An iterator that traverses trees of [Layout]
/// It keeps track of its current path in the tree
/// with a buffer of size `MAX_DEPTH`. This value needs
/// to be at least the depth of the tree.
pub struct LayoutIterator<'a, T, const MAX_DEPTH: usize> {
    root: Option<&'a Layout<'a, T>>,
    path: ArrayVec<(&'a Layout<'a, T>, usize), MAX_DEPTH>,
}

impl<'a, T, const MAX_DEPTH: usize> Iterator for LayoutIterator<'a, T, MAX_DEPTH> {
    type Item = ([usize; 2], Option<&'a T>);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(root) = self.root.take() {
            if !root.children.is_empty() {
                self.path.push((root, 0));
            }

            if root.meta.out_of_bounds {
                return None;
            }

            return Some(([0, 0], root.meta.data.as_ref()));
        }

        let (parent, parent_index) = self.path.last_mut()?;
        let Some(node) = parent.children.get(*parent_index) else {
            self.path.pop();
            return self.next();
        };

        if node.meta.out_of_bounds {
            return None;
        }

        *parent_index += 1;

        if !node.children.is_empty() {
            self.path.push((node, 0));
        }

        let position = self
            .path
            .iter()
            .map(|(p, _)| p.meta.computed_position)
            .chain([node.meta.computed_position])
            .fold(Vec2(0, 0), |acc, pos| Vec2(acc.0 + pos.0, acc.1 + pos.1));

        Some(([position.0, position.1], node.meta.data.as_ref()))
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
            meta: LayoutMeta {
                data: Some(text),
                computed_size: Vec2(len, 1),
                ..Default::default()
            },
            children: &mut [],
        }
    }

    /// Computes the layout of the current node and its children.
    ///
    /// This function recursively updates the inner state of the [Layout].
    pub fn compute(&mut self, bounds: [usize; 2]) {
        self.meta.computed_position = Vec2(0, 0);

        let flip_if_vertical = |vec: Vec2<usize>| match self.meta.orientation {
            Orientaiton::Horizontal => vec,
            Orientaiton::Vertical => vec.flip(),
        };

        if self.children.is_empty() {
            // No children, this node should have a computed_size already set
            return;
        }

        let [mut x, mut y] = [0, 0];
        let Vec2(width, height) = flip_if_vertical(Vec2::from(bounds));
        let mut tallest_within_line = 0;
        let mut max_width = 0;

        for child in self.children.iter_mut() {
            child.compute(flip_if_vertical(Vec2::from([width - x, height - y])).into());

            if flip_if_vertical(child.meta.computed_size).0 > width {
                // Give up, it won't ever fit
                child.meta.out_of_bounds = true;
                return;
            }

            let gap = if x > 0 { self.meta.gap } else { 0 };

            if x + gap + flip_if_vertical(child.meta.computed_size).0 > width {
                // Skip to the next line

                if y + flip_if_vertical(child.meta.computed_size).1 >= height {
                    // Out of lines, won't fit
                    child.meta.out_of_bounds = true;
                    return;
                }

                child.meta.computed_position = flip_if_vertical(Vec2(0, y + tallest_within_line));

                let consumed_width = flip_if_vertical(child.meta.computed_size).0;
                x = consumed_width;
                y += flip_if_vertical(child.meta.computed_size).1;
                tallest_within_line = flip_if_vertical(child.meta.computed_size).1;
                max_width = max_width.max(x);

                continue;
            }

            child.meta.computed_position = flip_if_vertical(Vec2(x + gap, y));

            let consumed_width = gap + flip_if_vertical(child.meta.computed_size).0;
            x += consumed_width;
            tallest_within_line =
                tallest_within_line.max(flip_if_vertical(child.meta.computed_size).1);
            max_width = max_width.max(x);
        }

        self.meta.computed_size = flip_if_vertical(Vec2(max_width, y + tallest_within_line))
    }

    /// Returns an iterator over all layout elements and their computed positions.
    ///
    /// This function yields tuples of `(position, data)` for every layout node that contains data,
    /// traversing the [Layout] tree in depth-first order.
    ///
    /// Be sure to call [Layout::compute()] before iterating.
    pub fn iter<const MAX_DEPTH: usize>(&'a self) -> impl Iterator<Item = ([usize; 2], &'a T)> {
        LayoutIterator::<'a, T, MAX_DEPTH> {
            root: Some(self),
            path: ArrayVec::new(),
        }
        .filter_map(|(pos, data)| data.map(|data| (pos, data)))
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

        layout.compute([20, 2]);
        insta::assert_snapshot!(layout.iter::<8>().collect::<TestBuffer>());
    }

    #[test]
    fn ignore_out_of_bound_columns() {
        let mut layout = Layout {
            meta: LayoutMeta::default().gap(1),
            children: &mut [Layout::text("1"), Layout::text("2")],
        };

        layout.compute([2, 1]);
        insta::assert_snapshot!(layout.iter::<8>().collect::<TestBuffer>());
    }

    #[test]
    fn ignore_out_of_bound_rows() {
        let mut layout = Layout {
            meta: LayoutMeta::default().vertical(),
            children: &mut [Layout::text("1"), Layout::text("2")],
        };

        layout.compute([1, 1]);
        insta::assert_snapshot!(layout.iter::<8>().collect::<TestBuffer>());
    }

    #[test]
    fn overflow_to_next_row() {
        let mut layout = Layout {
            meta: LayoutMeta::default(),
            children: &mut [Layout::text("1"), Layout::text("2")],
        };

        layout.compute([1, 2]);
        insta::assert_snapshot!(layout.iter::<8>().collect::<TestBuffer>());
    }

    #[test]
    fn overflow_to_next_column() {
        let mut layout = Layout {
            meta: LayoutMeta::default().vertical(),
            children: &mut [Layout::text("1"), Layout::text("2")],
        };

        layout.compute([2, 1]);
        insta::assert_snapshot!(layout.iter::<8>().collect::<TestBuffer>());
    }

    #[test]
    fn docs() {
        let mut layout = Layout {
            meta: LayoutMeta::default().vertical().gap(1),
            children: &mut [
                Layout::text("Layzer - A Lightweight Terminal Layout Library"),
                Layout {
                        meta: LayoutMeta::default(),
                        children: &mut "Layzer is a zero-heap-allocation layout library for building nested layouts with discrete positioning, and sizing. It's designed for terminal UIs, text-based interfaces, and any scenario where you need to compute positions and sizes of rectangular elements."
                            .split_word_bounds()
                            .map(Layout::text)
                            .collect::<Vec<_>>(),
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

        layout.compute([80, 60]);
        insta::assert_snapshot!(layout.iter::<16>().collect::<TestBuffer>());
    }

    #[test]
    fn gitu() {
        let mut layout = Layout {
            meta: LayoutMeta::default().vertical(),
            children: &mut [
                // Editor
                Layout {
                    meta: LayoutMeta::default().vertical(),
                    children: &mut [
                        Layout::text("On branch main"),
                        Layout::text("Your branch is up to date with 'origin/main'."),
                        Layout::text(""),
                        Layout::text("Untracked files"),
                        Layout::text("file.txt"),
                        Layout::text(""),
                    ],
                },
                // TODO The top Editor likely needs to grow to fill the space up until the menu
                // Menu
                Layout {
                    meta: LayoutMeta::default().vertical(),
                    children: &mut [
                        Layout::text("───────────────"),
                        // Menu columns
                        Layout {
                            meta: LayoutMeta::default().gap(2),
                            children: &mut [
                                Layout {
                                    meta: LayoutMeta::default().vertical(),
                                    children: &mut [
                                        Layout::text("Help"),
                                        Layout::text("Y Show Refs"),
                                        Layout::text("<tab> Toggle section"),
                                        Layout::text("k/<up> Up"),
                                        Layout::text("j/<down> Down"),
                                    ],
                                },
                                Layout {
                                    meta: LayoutMeta::default().vertical(),
                                    children: &mut [
                                        Layout::text("Submenu"),
                                        Layout::text("b Branch"),
                                        Layout::text("c Commit"),
                                        Layout::text("f Fetch"),
                                        Layout::text("h/? Help"),
                                    ],
                                },
                            ],
                        },
                    ],
                },
            ],
        };

        layout.compute([80, 60]);
        insta::assert_snapshot!(layout.iter::<16>().collect::<TestBuffer>());
    }
}
