// Typst template for LightLogR course certificates
// Inspired by https://github.com/royfrancis/quarto-typst-certificate
// Parameters are passed through Quarto metadata (params)

// Color palette
#let palette = (
  primary: rgb("#0B3259"),
  accent: rgb("#4CC9F0"),
  secondary: rgb("#F6B800"),
  neutral: rgb("#F7F9FC"),
  text: rgb("#1D1D1F"),
)

// Page styling
#set page(
  width: 297mm,
  height: 210mm,
  margin: 20mm,
  fill: palette.neutral,
)

#set text(font: "Helvetica Neue", size: 11pt, fill: palette.text)
#set heading(numbering: none, font: "Helvetica Neue", fill: palette.primary)

#let params = metadata("params")
#let participant = params.at("name", "Participant Name")
#let course_date = params.at("date", "Date of Completion")
#let level = params.at("level", "beginner")
#let use_case = params.at("use_case", "A Day in Daylight")
#let use_case_detail = params.at("use_case_detail", "")

#let base_outcomes = [
  "Walk through the LightLogR workflow: import → (pre-)processing → visualisation → metrics.",
  "Sense-check data quickly with tidy, reproducible steps and overview plots.",
  "Handle gaps, irregular sampling, and zero inflation with principled defaults.",
  "Add photoperiod information and derive interpretable summaries.",
]

#let use_case_outcomes = (
  "A Day in Daylight": [
    "Set up imports from multiple devices and time zones.",
    "Handle multiple time zones after import.",
    "Manage a large number of participants in a single analysis.",
    "Combine wearable data with participant-specific metadata.",
  ],
  "Case of high light sensitivity": [
    "Merge diary data with light exposure measurements.",
    "Calculate metrics based on sleep–wake cycles.",
    "Relate exposure metrics to sleep, performance, and wellbeing assessments.",
    "Assess compliance with Brown et al. (2022) recommendations for healthy light exposure.",
  ],
  "Therapy lamps": [
    "Merge participant protocol logs with wearable data.",
    "Analyze light exposure contingent on lighting conditions.",
    "Handle interruptions and deviations from the protocol.",
    "Craft advanced plots and tables for sub-day data windows.",
  ],
  "Visual experience": [
    "Import multimodal (light, distance, spectral, motion) data.",
    "Reconstruct spectral power distributions from sensor channels.",
    "Calculate spectrum-based metrics for interpretation.",
    "Simplify spatial grids of distance measurements and detect clusters.",
  ],
)

#let advanced_outcomes = use_case_outcomes.at(use_case, [])
#let displayed_outcomes = if level == "advanced" { base_outcomes + advanced_outcomes } else { base_outcomes }

#let subtitle = if level == "advanced" {
  "Advanced track — " + use_case + if use_case_detail != "" { " (" + use_case_detail + ")" } else { "" }
} else {
  "Beginner track"
}

#let signature_line(name) = align(left)[
  #line(length: 70mm, stroke: (paint: palette.primary, thickness: 1.2pt))
  #v(4pt)
  #text(size: 9pt, weight: "bold", fill: palette.primary)[#name]
]

#pagebreak()
#show: doc => {
  // Header band with logos
  align(center)[
    #box(
      width: 100%,
      fill: palette.primary,
      inset: 12pt,
      radius: 10pt,
    )[
      #grid(columns: (1fr, 1fr, 1fr), gutter: 12pt)[
        align(left)[#text(fill: palette.neutral, weight: "bold", size: 12pt)[Open & Reproducible Analysis]],
        align(center)[#text(fill: palette.neutral, weight: "bold", size: 14pt)[LightLogR Course Series]],
        align(right)[#text(fill: palette.neutral, size: 10pt)[https://tscnlab.github.io/LightLogR_webinar/]],
      ]
    ]
  ]

  #v(14pt)

  // Main certificate panel
  #box(
    width: 100%,
    fill: white,
    inset: 18pt,
    stroke: (paint: palette.accent, thickness: 1.3pt),
    radius: 14pt,
    shadow: (x: 1pt, y: 2pt, blur: 8pt, color: luma(65%)),
  )[
    #align(center)[
      #text(size: 26pt, weight: "bold", fill: palette.primary)[Certificate of Completion]
      #v(6pt)
      #line(length: 50%, stroke: (paint: palette.accent, thickness: 2pt))
      #v(10pt)
      #text(size: 13pt)[This certifies that]
      #v(4pt)
      #text(size: 24pt, weight: "bold", fill: palette.secondary)[#participant]
      #v(6pt)
      #text(size: 12pt)[has successfully completed the]
      #text(size: 15pt, weight: "semibold")[Open and reproducible analysis of light exposure and visual experience data]
      #v(6pt)
      #text(size: 12pt, fill: palette.primary)[#subtitle]
      #v(10pt)
      #text(size: 11pt)[Awarded on #course_date]
    ]

    #v(12pt)
    #line(length: 100%, stroke: (paint: palette.primary, thickness: 0.8pt))
    #v(10pt)

    #text(size: 14pt, weight: "bold", fill: palette.primary)[Learning outcomes]
    #v(6pt)
    #for item in displayed_outcomes {
      - #item
    }

    #v(12pt)
    #line(length: 100%, stroke: (paint: palette.accent, thickness: 0.6pt))
    #v(8pt)

    #grid(columns: (1fr, 1fr), gutter: 16pt)[
      align(left)[
        #text(size: 12pt, weight: "bold", fill: palette.primary)[Course leads]
        #v(4pt)
        Manuel Spitschan\\
        Technical University of Munich & MPI for Biological Cybernetics\\
        ORCID: 0000-0002-8572-9268
        #v(10pt)
        Johannes Zauner\\
        Technical University of Munich & MPI for Biological Cybernetics\\
        ORCID: 0000-0003-2171-4566
      ],
      align(right)[
        #text(size: 12pt, weight: "bold", fill: palette.primary)[Course & resources]
        #v(4pt)
        #text(size: 10pt)[Course page: https://tscnlab.github.io/LightLogR_webinar/]
        #text(size: 10pt)[Repository: https://github.com/tscnlab/LightLogR_webinar]
        #text(size: 10pt)[Package: https://cran.r-project.org/package=LightLogR]
      ],
    ]

    #v(14pt)
    #grid(columns: (1fr, 1fr), gutter: 24pt)[
      signature_line("Manuel Spitschan"),
      signature_line("Johannes Zauner"),
    ]
  ]

  #v(10pt)
  align(center)[
    #text(size: 9pt, fill: palette.text)[This certificate acknowledges commitment to open, reproducible analysis of light exposure and visual experience data using LightLogR.]
  ]
}
