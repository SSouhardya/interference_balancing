# Low-rank Covariate Balancing Estimators under Interference
A repository for replicating the simulations of "Low-rank Covariate Balancing Estimators under Interference"

To replicate the simulation results, follow these steps:
1. Create the folders 'output_files' and 'Figures' in your home directory
2. Copy the .R and .sh files to your home directory
3. Run the file submit_jobs.sh. It is designed to submit jobs in a SLURM cluster corresponding to the various experimental settings. In case you hit the maximum limit of the number of jobs you can submit, the script will keep record of the settings it could not submit. Hence, run exactly the same script later when some of the submitted jobs are completed and it will run the jobs it previously could not submit.
4. Once all the jobs have finished running, run the script combine_jobs.sh. It will create summary files inside 'output_files'.
5. Finally, run the R file plot_codes.R. It will create the figures and store them in the folder 'Figures'

## Reference
```
@article{SS-ea:2026,
      title={Low-rank Covariate Balancing Estimators under Interference}, 
      author={Souhardya Sengupta and Kosuke Imai and Georgia Papadogeorgou},
      year={2026},
      eprint={2512.13944},
      archivePrefix={arXiv},
      primaryClass={stat.ME},
      url={https://arxiv.org/abs/2512.13944}, 
}
```
