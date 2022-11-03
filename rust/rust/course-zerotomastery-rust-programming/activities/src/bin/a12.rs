// Topic: Implementing functionality with the impl keyword
//
// Requirements:
// * Print the characteristics of a shipping box
// * Must include dimensions, weight, and color
//
// Notes:
// * Use a struct to encapsulate the box characteristics
// * Use an enum for the box color
// * Implement functionality on the box struct to create a new box
// * Implement functionality on the box struct to print the characteristics

enum Color {
    Red,
    Green,
    Blue,
}

impl Color {
    fn print(&self) {
        match self {
            Color::Red => println!("color=red"),
            Color::Green => println!("color=green"),
            Color::Blue => println!("color=blue"),
        }
    }
}

struct Dimensions {
    width: f32,
    height: f32,
    depth: f32,
}

impl Dimensions {
    fn new(width: f32, height: f32, depth: f32) -> Self {
        Self {
            width: width,
            height: height,
            depth: depth,
        }
    }

    fn print(&self) {
        println!(
            "width={:?}, height={:?}, depth={:?}",
            self.width, self.height, self.depth
        )
    }
}

struct ShippingBox {
    weight: f32,
    color: Color,
    dimensions: Dimensions,
}

impl ShippingBox {
    fn new(color: Color, weight: f32, dimensions: Dimensions) -> Self {
        Self {
            color: color,
            weight: weight,
            dimensions: dimensions,
        }
    }

    fn print(&self) {
        self.color.print();
        self.dimensions.print();
        println!("weight={:?}", self.weight)
    }
}

fn main() {
    let small_bx = Dimensions::new(1.0, 2.5, 1.0);
    let bx = ShippingBox::new(Color::Red, 10.0, small_bx);
    bx.print();
}
