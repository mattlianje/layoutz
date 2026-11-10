//! ratatui version of ../Dashboard.hs, driven by ../Main.hs.
//!
//!     ratatui-dashboard render    ms per frame
//!     ratatui-dashboard idle      run the app, redraw on "+"
//!     ratatui-dashboard busy      same, plus a tick every 10ms
//!     ratatui-dashboard version   ratatui version
//!
//! Layout is fixed rectangles, so box sizes are computed from the content.
//! Table has no column borders: columns are space-separated in a bordered block.
use std::hint::black_box;
use std::io;
use std::time::{Duration, Instant};

use ratatui::buffer::Buffer;
use ratatui::crossterm::event::{self, Event, KeyCode, KeyEventKind, KeyModifiers};
use ratatui::layout::{Constraint, Layout, Rect};
use ratatui::style::{Color, Style};
use ratatui::text::Line;
use ratatui::widgets::{Block, Padding, Paragraph, Row, Table, Widget};

const WIDTH: u16 = 200;
const HEIGHT: u16 = 60;

fn boxed(title: &str) -> Block<'_> {
    Block::bordered().title(title).padding(Padding::horizontal(1))
}

fn bar(label: &str, p: f64) -> String {
    let filled = (p * 20.0) as usize;
    format!(
        "{label} [{}{}] {}%",
        "█".repeat(filled),
        "─".repeat(20 - filled),
        (p * 100.0) as i64
    )
}

fn width_of(lines: &[String]) -> u16 {
    lines.iter().map(|l| l.chars().count()).max().unwrap_or(0) as u16
}

fn dashboard(k: i64, area: Rect, buf: &mut Buffer) {
    let ids: Vec<String> = (1..=40).map(|i: i64| i.to_string()).collect();
    let names: Vec<String> = (1..=40).map(|i| format!("item{}", i + k)).collect();
    let values: Vec<String> = (1..=40).map(|i| (i * k).to_string()).collect();
    let widths = [
        width_of(&ids).max(2),
        width_of(&names).max(4),
        width_of(&values).max(5),
    ];
    let rows = (0..40).map(|i| Row::new([ids[i].as_str(), names[i].as_str(), values[i].as_str()]));
    let table = Table::new(rows, widths.map(Constraint::Length))
        .header(Row::new(["id", "name", "value"]))
        .column_spacing(1)
        .block(Block::bordered());
    // outer border + padding + table border + columns + gaps
    let table_w = 2 + 2 + 2 + widths.iter().sum::<u16>() + 2;
    let table_h = 2 + 2 + 1 + 40;

    let bars: Vec<String> = (1..=15)
        .map(|i| bar(&format!("b{i}"), ((i * k) % 100) as f64 / 100.0))
        .collect();
    let logs: Vec<String> = (1..=20)
        .map(|j| format!("line {} lorem ipsum dolor sit amet", j + k))
        .collect();
    let right_w = 4 + width_of(&bars).max(width_of(&logs));

    let [left, _, right] = Layout::horizontal([
        Constraint::Length(table_w),
        Constraint::Length(1),
        Constraint::Length(right_w),
    ])
    .areas(area);
    let [top, bottom] = Layout::vertical([Constraint::Length(15 + 2), Constraint::Length(20 + 2)])
        .areas(right);

    let outer = boxed("Table");
    let left = Rect { height: table_h.min(left.height), ..left };
    let inner = outer.inner(left);
    outer.render(left, buf);
    table.render(inner, buf);

    let green = Style::new().fg(Color::Green);
    Paragraph::new(bars.into_iter().map(Line::from).collect::<Vec<_>>())
        .block(boxed("Bars").style(green))
        .style(green)
        .render(top, buf);

    let cyan = Style::new().fg(Color::Cyan);
    Paragraph::new(logs.into_iter().map(|l| Line::styled(l, cyan)).collect::<Vec<_>>())
        .block(boxed("Log"))
        .render(bottom, buf);
}

fn run(busy: bool) -> io::Result<()> {
    let mut terminal = ratatui::init();
    let tick = Duration::from_millis(10);
    let mut next_tick = Instant::now() + tick;
    let mut k: i64 = 1;
    let result = (|| loop {
        terminal.draw(|f| dashboard(k, f.area(), f.buffer_mut()))?;
        loop {
            let ready = if busy {
                event::poll(next_tick.saturating_duration_since(Instant::now()))?
            } else {
                true // block in read
            };
            if !ready {
                k += 1;
                next_tick += tick;
                break;
            }
            if let Event::Key(key) = event::read()? {
                if key.kind != KeyEventKind::Press {
                    continue;
                }
                match key.code {
                    KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                        return Ok(());
                    }
                    KeyCode::Char('+') => {
                        k += 1;
                        break;
                    }
                    _ => {}
                }
            }
        }
    })();
    ratatui::restore();
    result
}

/// ms per frame, widgets -> cell buffer (what `Terminal::draw` fills before
/// diffing). 100 warmup frames, then time 2000.
fn render_time() -> f64 {
    let area = Rect::new(0, 0, WIDTH, HEIGHT);
    let mut buf = Buffer::empty(area);
    let mut frame = |k: i64| {
        buf.reset();
        dashboard(k, area, &mut buf);
        black_box(&buf);
    };
    for k in 1..=100 {
        frame(k);
    }
    const FRAMES: i64 = 2000;
    let start = Instant::now();
    for k in 1..=FRAMES {
        frame(k);
    }
    start.elapsed().as_secs_f64() * 1000.0 / FRAMES as f64
}

fn main() {
    let mode = std::env::args().nth(1).unwrap_or_default();
    match mode.as_str() {
        "render" => println!("{:.4}", render_time()),
        "version" => println!("{}", env!("RATATUI_VERSION")),
        "idle" | "busy" => {
            if let Err(e) = run(mode == "busy") {
                eprintln!("{e}");
                std::process::exit(1);
            }
        }
        _ => {
            eprintln!("usage: ratatui-dashboard render|idle|busy|version");
            std::process::exit(2);
        }
    }
}
