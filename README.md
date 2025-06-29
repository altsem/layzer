# Layzer - A Lightweight Terminal Layout Library

**Work-in-progress, unfinished and broken, be warned**

```
Layzer is a zero-heap-allocation layout library for
building nested layouts with discrete positioning
and sizing.
It's designed for terminal UIs, text-based interfaces,
and any scenario where you need to compute positions and
sizes of rectangular elements.

- Zero heap allocations -           - Hierarchical layouts -
All layout computation is done      Support for nesting layouts
without dynamic memory allocation.  and arranging children within them.

- Flexible sizing -               - Axis-oriented layouts -
Auto-sizing based on content or   Arrange  children  horizontally
fixed dimensions.                 children
                                  vertically
```
## Quick start
```rust
use layzer::{Layout, LayoutMeta, Axis};

// Create a text element
let text_layout = Layout::text("Hello, World!");

// Create a container with children
let mut container = Layout {
    meta: LayoutMeta::default().vertical().gap(1),
    children: &mut [
        Layout::text("First line"),
        Layout::text("Second line"),
    ],
};

// Compute layout within 80x24 bounds
container.compute([80, 24]);

// Iterate over computed positions and data with a stack-size of 8
for (position, text) in container.iter::<8>() {
    println!("Text '{}' at position {:?}", text, position);
}
```


## Planned features:
- Text-wrap of various kind
