#TO SCHEDULE JOBS

cd ~/spillover_balance/fixed_experiments/additive_ci
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/additive_ci_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/additive_ci_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/additive_est
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/additive_est_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/additive_est_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh


cd ~/spillover_balance/fixed_experiments/graph_ci
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_ci_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_ci_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_ci_adapt
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_ci_lopsided_adapt
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_ci_snr_adapt
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_est
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_est_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_est_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh





#cd ~/spillover_balance/fixed_experiments/graph_ME_check
#rm *.err
#rm *.out
#sbatch --array=501-1500 run_spillover.sh






cd ~/spillover_balance/fixed_experiments/stratified_ci
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/stratified_ci_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/stratified_ci_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/stratified_est
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/stratified_est_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/stratified_est_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh


#TO COMBINE JOB FILES

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/additive_ci

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/additive_ci_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/additive_ci_snr

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/additive_est

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/additive_est_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/additive_est_snr



module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci_snr



module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci_adapt

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci_lopsided_adapt

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci_snr_adapt





module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ME_check



module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/graph_est

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/graph_est_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/graph_est_snr



module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/stratified_ci

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/stratified_ci_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/stratified_ci_snr

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/stratified_est

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/stratified_est_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/stratified_est_snr



#TO SCHEDULE JOBS

cd ~/spillover_balance/fixed_experiments/additive_ci
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/additive_ci_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/additive_ci_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/additive_est
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/additive_est_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/additive_est_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh




cd ~/spillover_balance/fixed_experiments/graph_ci
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_ci_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_ci_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_ci_adapt
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_ci_lopsided_adapt
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_ci_snr_adapt
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_est
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_est_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/graph_est_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh





cd ~/spillover_balance/fixed_experiments/graph_ME_check
rm *.err
rm *.out
sbatch --array=501-1500 run_spillover.sh






cd ~/spillover_balance/fixed_experiments/stratified_ci
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/stratified_ci_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/stratified_ci_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/stratified_est
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/stratified_est_lopsided
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh

cd ~/spillover_balance/fixed_experiments/stratified_est_snr
rm *.err
rm *.out
rm *.list
sbatch --array=1-1500 run_spillover.sh


#TO COMBINE JOB FILES

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/additive_ci

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/additive_ci_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/additive_ci_snr

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/additive_est

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/additive_est_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/additive_est_snr



module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci_snr



module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci_adapt

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci_lopsided_adapt

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/graph_ci_snr_adapt





#module load R
#cd ~/spillover_balance
#Rscript combine_results_general.R 1 fixed_experiments/graph_ME_check



module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/graph_est

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/graph_est_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/graph_est_snr



module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/stratified_ci

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/stratified_ci_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 1 fixed_experiments/stratified_ci_snr

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/stratified_est

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/stratified_est_lopsided

module load R
cd ~/spillover_balance
Rscript combine_results_general.R 0 fixed_experiments/stratified_est_snr



