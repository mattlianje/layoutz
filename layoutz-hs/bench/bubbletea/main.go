// Bubble Tea + lipgloss version of ../Dashboard.hs, driven by ../Main.hs.
//
//	bubbletea-dashboard render   ms per frame
//	bubbletea-dashboard idle     run the app, redraw on "+"
//	bubbletea-dashboard busy     same, plus a tick every 10ms
//	bubbletea-dashboard version  Bubble Tea version
package main

import (
	"fmt"
	"os"
	"runtime/debug"
	"strings"
	"time"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
	"charm.land/lipgloss/v2/table"
)

const width, height = 200, 60

var (
	boxStyle = lipgloss.NewStyle().Border(lipgloss.NormalBorder()).Padding(0, 1)
	green    = lipgloss.NewStyle().Foreground(lipgloss.Color("2"))
	cyan     = lipgloss.NewStyle().Foreground(lipgloss.Color("6"))
)

// lipgloss has no titled border, so the title goes on the first line inside.
func box(title, body string) string { return boxStyle.Render(title + "\n" + body) }

func bar(label string, p float64) string {
	filled := int(p * 20)
	return fmt.Sprintf("%s [%s%s] %d%%", label, strings.Repeat("█", filled), strings.Repeat("─", 20-filled), int(p*100))
}

func dashboard(k int) string {
	rows := make([][]string, 0, 40)
	for i := 1; i <= 40; i++ {
		rows = append(rows, []string{fmt.Sprint(i), fmt.Sprintf("item%d", i+k), fmt.Sprint(i * k)})
	}
	tbl := table.New().Border(lipgloss.NormalBorder()).Headers("id", "name", "value").Rows(rows...).Render()

	bars := make([]string, 0, 15)
	for i := 1; i <= 15; i++ {
		bars = append(bars, bar(fmt.Sprintf("b%d", i), float64((i*k)%100)/100))
	}

	logs := make([]string, 0, 20)
	for j := 1; j <= 20; j++ {
		logs = append(logs, cyan.Render(fmt.Sprintf("line %d lorem ipsum dolor sit amet", j+k)))
	}

	right := lipgloss.JoinVertical(lipgloss.Left,
		green.Render(box("Bars", strings.Join(bars, "\n"))),
		box("Log", strings.Join(logs, "\n")))
	return lipgloss.JoinHorizontal(lipgloss.Top, box("Table", tbl), " ", right)
}

type next struct{}

type model struct {
	k    int
	busy bool
}

func every10ms() tea.Cmd {
	return tea.Tick(10*time.Millisecond, func(time.Time) tea.Msg { return next{} })
}

func (m model) Init() tea.Cmd {
	if m.busy {
		return every10ms()
	}
	return nil
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyPressMsg:
		switch msg.String() {
		case "ctrl+c":
			return m, tea.Quit
		case "+":
			m.k++
		}
	case next:
		m.k++
		return m, every10ms()
	}
	return m, nil
}

func (m model) View() tea.View {
	v := tea.NewView(dashboard(m.k))
	v.AltScreen = true
	return v
}

// 100 warmup frames, then time 2000.
func renderTime() float64 {
	for k := 1; k <= 100; k++ {
		_ = dashboard(k)
	}
	const frames = 2000
	start := time.Now()
	for k := 1; k <= frames; k++ {
		_ = dashboard(k)
	}
	return float64(time.Since(start).Microseconds()) / 1000 / frames
}

func version() string {
	if info, ok := debug.ReadBuildInfo(); ok {
		for _, dep := range info.Deps {
			if dep.Path == "charm.land/bubbletea/v2" {
				return dep.Version
			}
		}
	}
	return "(unknown version)"
}

func main() {
	mode := ""
	if len(os.Args) > 1 {
		mode = os.Args[1]
	}
	switch mode {
	case "render":
		fmt.Printf("%.4f\n", renderTime())
	case "version":
		fmt.Println(version())
	case "idle", "busy":
		if _, err := tea.NewProgram(model{k: 1, busy: mode == "busy"}).Run(); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	default:
		fmt.Fprintln(os.Stderr, "usage: bubbletea-dashboard render|idle|busy|version")
		os.Exit(2)
	}
}
