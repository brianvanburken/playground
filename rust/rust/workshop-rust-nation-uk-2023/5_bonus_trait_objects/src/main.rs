use std::fmt::Write;

fn main() {
    let mut window = Window::new("Rust GUI demo");
    window.add_widget(Box::new(Label::new("This is a small label in the GUI.")));
    // window.add_widget(Box::new(Button::new("Click me!")));
    window.draw();
}

trait Widget {
    fn width(&self) -> usize;

    fn draw_into(&self, buffer: &mut dyn Write);

    fn draw(&self) {
        let mut buffer = String::new();
        self.draw_into(&mut buffer);
        println!("{}", &buffer);
    }
}

struct Window {
    title: String,
    widgets: Vec<Box<impl Widget>>,
}

impl Window {
    fn new(title: &str) -> Self {
        Self {
            title: title.to_owned(),
            widgets: vec![],
        }
    }

    fn add_widget(&mut self, widget: Box<dyn Widget>) {
        self.widgets.push(widget)
    }
}

impl Widget for Window {
    fn width(&self) -> usize {
        todo!()
    }

    fn draw_into(&self, buffer: &mut dyn Write) {
        todo!()
    }
}

struct Label {
    title: String,
}

impl Label {
    fn new(title: &str) -> Self {
        Self {
            title: title.to_owned(),
        }
    }
}

struct Button {
    label: Label,
}
