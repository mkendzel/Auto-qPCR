## Project Data Repository
- Each experiment is stored in its own project folder, created when the app is launched.
- All subsequent QC steps and file edits must use the same project name to continue working within that project.

## Internal Folder Structure
- **raw/**  
  Original input files as imported into the app. These files are never modified and serve as a reference to the original PCR machine output.

- **processed/**  
  Cleaned or edited versions of data files that were generated after quality control filtering. These files were then used for downstream analysis.

- **qc/**  
  Quality control outputs, including QC tables and data files that can be edited for repload. Once files are reploaded, they are stored in processed.

- **exports/**  
  Final user-requested outputs intended for downstream analysis or visualization (e.g., Prism-ready files).

- **meta_data/**  
  Experiment-level metadata, full calculation tables used to generate QC results.

- **logs/**  
  Time-stamped run logs capturing app actions, inputs, and state changes for reproducibility and auditing.
