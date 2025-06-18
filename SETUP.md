# SquiggleConf 2025 Notes Repository

## Repository Structure

```
squiggleconf-2025/
├── README.org
├── schedule.org
├── sessions/
│   ├── index.org
│   ├── make-tools-people-love.org
│   ├── electron-internal-tooling.org
│   ├── cross-browser-devtools.org
│   ├── lockfiles-phd.org
│   ├── javascript-framework-cage-match.org
│   ├── effect-type-safe-errors.org
│   ├── typescript-to-go.org
│   ├── rolldown-vite-rust.org
│   ├── snapshot-tests-gleam.org
│   ├── aspire-local-dev.org
│   ├── quality-code-samples.org
│   ├── source-maps-magic.org
│   ├── accessibility-storybook.org
│   ├── aot-typescript-wasm.org
│   ├── jsr-package-registry.org
│   ├── wasi-building-blocks.org
│   ├── why-we-made-tsdoc.org
│   ├── tech-broke-my-heart.org
│   └── zero-to-squiggle-lsp.org
├── speakers/
│   ├── index.org
│   └── speaker-notes/
├── tools/
│   ├── setup.org
│   ├── demo-projects/
│   └── scripts/
├── diagrams/
│   └── .gitkeep
├── networking/
│   ├── boston-ts-club.org
│   └── michigan-typescript.org
└── exports/
    ├── html/
    ├── pdf/
    └── markdown/
```

## Key Template Files

### README.org
```org
#+TITLE: SquiggleConf 2025 Notes
#+AUTHOR: Your Name
#+DATE: September 18-19, 2025
#+STARTUP: overview
#+PROPERTY: header-args :mkdirp yes

* SquiggleConf 2025 🌊

Web dev tooling conference at the New England Aquarium, Boston.

[[file:diagrams/squiggleconf-overview.png]]

#+begin_src mermaid :file diagrams/squiggleconf-overview.png
graph TB
    SC[SquiggleConf 2025]
    SC --> Tools[Tooling Sessions]
    SC --> Network[Networking]
    SC --> Community[Community Building]
    
    Tools --> TS[TypeScript Tools]
    Tools --> Build[Build Systems]
    Tools --> LSP[Language Servers]
    Tools --> Testing[Testing Tools]
    Tools --> DX[Developer Experience]
    
    Network --> BTSC[Boston TS Club]
    Network --> MTS[Michigan TypeScript]
    
    Community --> Teen[Teen Coding]
    Community --> Access[Accessibility]
#+end_src

** Conference Details
- *Date*: September 18-19, 2025
- *Location*: Simons IMAX Theater, New England Aquarium, Boston
- *Website*: [[https://2025.squiggleconf.com/][2025.squiggleconf.com]]
- *Focus*: Latest and greatest in web dev tooling
- *Organizer*: Squiggle.Tools (nonprofit)

** Session Categories
- [[file:sessions/index.org][All Sessions]] - Full speaker lineup
- Build Tools & Bundlers
- Language Servers & IDEs
- Testing & Quality
- Developer Experience
- WebAssembly & Performance

** Quick Navigation
- [[file:schedule.org][Conference Schedule]]
- [[file:speakers/index.org][Speaker Directory]]
- [[file:tools/setup.org][Development Setup]]
- [[file:networking/boston-ts-club.org][Boston TS Club]]

** Repository Features
- Org-mode notes with Babel integration
- Executable code examples (`:tangle` enabled)
- Mermaid diagrams for architecture visualization
- Speaker insights and key takeaways
- Demo projects from sessions
```

### schedule.org
```org
#+TITLE: SquiggleConf 2025 Schedule
#+STARTUP: overview

* Day 1 - Thursday, September 18, 2025

** Morning Sessions
*** [[file:sessions/make-tools-people-love.org][Make Tools That People Love]]
Anthony Fu - Open sourceror and design engineer
Focus: Developer experience and tool design philosophy

*** [[file:sessions/electron-internal-tooling.org][Electron's Internal Tooling: Open Source at Scale]]
Shelley Vohr - Core eng @electron; collaborator @nodejs
Focus: Managing large-scale open source projects

*** [[file:sessions/cross-browser-devtools.org][Cross-Browser DevTools with WebExtensions]]
Oliver Dunk - Chrome Extensions @ Google, Editor @ W3C WebExtensions CG
Focus: Building developer tools that work everywhere

** Afternoon Sessions
*** [[file:sessions/lockfiles-phd.org][The Lockfiles PhD You Never Got]]
Pete Gonzalez - Co-creator of Lockfile Explorer, engineer at TikTok
Focus: Deep dive into package management

*** [[file:sessions/javascript-framework-cage-match.org][JavaScript Framework Cage Match]]
Amy Dutton - RedwoodJS lead maintainer
Focus: Framework comparisons and tooling

*** [[file:sessions/effect-type-safe-errors.org][Effect: Type-Safe Errors and Dependency Injection]]
Mattia Manzati - Founding engineer, Effectful Technologies
Focus: Advanced TypeScript patterns

* Day 2 - Friday, September 19, 2025

** Morning Sessions
*** [[file:sessions/typescript-to-go.org][How and Why We Ported TypeScript to Go]]
Jake Bailey - TypeScript team
Focus: Performance and language implementation

*** [[file:sessions/rolldown-vite-rust.org][Rolldown: How Vite Bundles at the Speed of Rust]]
Alexander Lichter - Developer on Nuxt and void(0) devrel engineer
Focus: Next-gen bundler performance

*** [[file:sessions/source-maps-magic.org][Source Maps: How Does the Magic Work?]]
Nicolò Ribaudo - Babel maintainer and TC39 representative at Igalia
Focus: Debugging and transpilation

** Afternoon Sessions
*** [[file:sessions/jsr-package-registry.org][JSR: Designing Package Registry Module Resolution]]
David Sherret - Deno developer; creator of ts-morph, dprint, ts-ast-viewer
Focus: Modern package management

*** [[file:sessions/zero-to-squiggle-lsp.org][Zero to Squiggle with the Language Server Protocol]]
TJ DeVries - Neovim core team
Focus: Building language servers from scratch

*** [[file:sessions/tech-broke-my-heart.org][Tech Broke My Heart]]
Michelle Bakels - Software developer, program director of developer health at G2i
Focus: Developer wellness and sustainability
```

### sessions/typescript-to-go.org (Example Session Template)
```org
#+TITLE: How and Why We Ported TypeScript to Go
#+DATE: <2025-09-19 Fri 09:00>
#+PROPERTY: header-args:python :session *ts-to-go* :results output
#+PROPERTY: header-args:go :results output
#+PROPERTY: header-args:mermaid :file ../diagrams/ts-to-go-%%N.png

* Session Information
- *Speaker*: Jake Bailey (TypeScript team)
- *Time*: Day 2, 9:00 AM
- *Topics*: Performance, Language Implementation, Compiler Architecture

* Pre-Session Research
- [[https://github.com/microsoft/TypeScript][TypeScript GitHub]]
- Previous work on TypeScript performance

* Architecture Overview

** TypeScript Compiler Pipeline

#+begin_src mermaid :file ../diagrams/ts-compiler-pipeline.png
graph LR
    Source[TS Source] --> Scanner
    Scanner --> Parser
    Parser --> AST[AST]
    AST --> Binder
    Binder --> Checker[Type Checker]
    Checker --> Transform[Transformer]
    Transform --> Emit[Emitter]
    Emit --> JS[JavaScript]
    
    style Checker fill:#f9f,stroke:#333,stroke-width:4px
#+end_src

** Performance Comparison

#+begin_src python :tangle ../tools/scripts/perf-comparison.py
#!/usr/bin/env python3
"""Compare TypeScript vs Go implementation performance metrics."""

import matplotlib.pyplot as plt
import numpy as np
from dataclasses import dataclass
from typing import List

@dataclass
class PerfMetric:
    operation: str
    ts_time_ms: float
    go_time_ms: float
    
    @property
    def speedup(self) -> float:
        return self.ts_time_ms / self.go_time_ms

# Session data will be collected here
metrics = [
    PerfMetric("Parse 1000 files", 0, 0),
    PerfMetric("Type check large project", 0, 0),
    PerfMetric("Incremental compilation", 0, 0),
    PerfMetric("Memory usage (MB)", 0, 0),
]

def plot_performance_comparison(metrics: List[PerfMetric]):
    """Create performance comparison visualization."""
    operations = [m.operation for m in metrics]
    ts_times = [m.ts_time_ms for m in metrics]
    go_times = [m.go_time_ms for m in metrics]
    
    x = np.arange(len(operations))
    width = 0.35
    
    fig, ax = plt.subplots(figsize=(10, 6))
    ts_bars = ax.bar(x - width/2, ts_times, width, label='TypeScript')
    go_bars = ax.bar(x + width/2, go_times, width, label='Go Port')
    
    ax.set_ylabel('Time (ms)')
    ax.set_title('TypeScript vs Go Implementation Performance')
    ax.set_xticks(x)
    ax.set_xticklabels(operations, rotation=45, ha='right')
    ax.legend()
    
    # Add speedup annotations
    for i, metric in enumerate(metrics):
        if metric.go_time_ms > 0:
            speedup = metric.speedup
            ax.text(i, max(ts_times[i], go_times[i]) * 1.05, 
                   f'{speedup:.1f}x', ha='center')
    
    plt.ti
