# Waiting-list access management

Research and conference-presentation project for **Beyond Digitalization: Priority Erosion and the Transition from Waiting Lists to Active Healthcare Access Management** (Ogorevc & Prevolnik Rupel).

## Contents

- `import waitlist data NIJZ.R`: existing NIJZ import and transformation script (local source paths must be configured).
- `presentation.qmd`: Quarto / Reveal.js conference presentation (English; empirical figures currently placeholders).
- `styles.scss`: slide styling.
- `.gitignore`: prevents accidental commit of data and local outputs.

## Rendering

Install [Quarto](https://quarto.org/) and open this folder in RStudio. Run:

```bash
quarto render presentation.qmd
```

Or use RStudio's **Render** button. The output is `presentation.html` (ignored by Git).

## Analysis next steps

1. Make the import script portable (replace hard-coded local root path).
2. Verify which input periods are included, checking gaps and duplicates.
3. Inspect naming and units for waiting-time indicators; assess comparability across years.
4. Define priority shares with clear denominators (waiting stock vs realized flow).
5. Generate empirical charts and replace the slides' placeholders.
6. Distinguish **priority inflation** (classification mix) from **priority erosion** (loss of effective access differentiation).

**Data policy:** Do not push raw NIJZ exports, person-level data, or sensitive local files to this public repository.
