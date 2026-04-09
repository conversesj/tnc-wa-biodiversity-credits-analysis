# tnc-wa-biodiversity-credits-analysis

Code and analysis workflows for The Nature Conservancy Washington Biodiversity Credits project, including bird acoustic detections, wildlife camera data, and supporting scripts for data processing and review preparation across Hoh, Clearwater, and Ellsworth.

## Repository structure

- `scripts/acoustic/`: acoustic detection workflows, including merging, thresholding, QC, minute-bin aggregation, and site-level comparison
- `scripts/camera/`: wildlife camera event-based analysis
- `scripts/utils/`: helper scripts for file extraction and batch processing
- `notebooks/`: exploratory and presentation-oriented notebooks

## Main acoustic workflow

1. Merge detections within plot
2. Merge plots within site
3. Apply Gio calibration thresholds
4. Run acoustic QC summaries
5. Compare detections across sites
6. Aggregate detections to minute bins
7. Prepare random acoustic samples for manual review

## Notes

This repository is primarily for code organization, reproducible workflows, and analysis support. Large raw input files and exported outputs are not stored here unless added later.
