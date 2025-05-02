Step 1: Using raw data to make a nexus file of inflorescence types for Bomarea
    a. run "bomareacode.R" on "bomarea traits.xlsx"
        this code takes the measurements of herbarium speciemens and data from
        the excel sheet and categorizes the inflorescences into 3 types with the
        numerical labels: 0,1,2
    b. you should have "type.nexus" in your data file

Step 2: Generate a tree using type.nexus and bom_only_MAP.tre
    a. Set working directory in zsh
        cd ~/Desktop/bomarea_traits/
    b. Open RevBayes in zsh and execute script
        i. enter rb in your terminal to run RevBayes
        ii. run "infl_type_ard.Rev" script until "->"
            this script will run the Markov Chain Monte Carlo (MCMC) on both the .tree file
            (which has molecular data) and the .nexus file (which has our trait of
            interest of interest), there is more information if you would like to more at 
            https://revbayes.github.io/tutorials/mcmc/archery.html (super useful). Also, you can
            run the entire script and it will stop at "->"; that's why that is there, I
            just don't do it out of habit
        iii. let it run until it's done
        iv. keep window open
    c. In zsh combine and remove first 10% of data
        i. in another terminal window change directory to ~/Desktop/bomarea_traits/output
        ii. run code to combine runs (.txt files)
            awk 'FNR == 1 && NR != 1 { next } { print }' infl_type_ard_states_run_1.txt infl_type_ard_states_run_2.txt > infl_type_ard_states_combined.txt
                the MCMC produces 2 sets of results, we want to combine those 2 without
                repeating any of the column names
        iii.run code to remove first 10% of code
            total_lines=$(wc -l < infl_type_ard_states_combined.txt)
            skip_lines=$((total_lines / 10))
            awk -v skip="$skip_lines" 'NR > skip || NR == 1' infl_type_ard_states_combined.txt > infl_type_ard_states_combined_trimmed.txt
                the first 10% is the analysis callibrating, so we "burnin 10" to remove
                the first 10%
    d. Return to RevBayes window (the one where you ran the MCMC)
        i. execute the two lines after "->"
            calculates ancestral states (pie charts at the nodes of the phylogeny)
            and a file so that you can make nice figures
        ii. you should have "infl_type_ase_ard.tree" in your output folder
                new tree to plot

Step 3: Plot tree in R
    a. Run "plotresults.R" with "infl_type_ase_ard.tree" and save the plot as a .png
        makes a phylogeny with our trait of interest, species name, legend, and each node's ancestral states

Step 4: Create a Violin Plot using rates
    a. run "plotviolinrates.R" and save plot as .png
        makes a violin plot with the rates of transitions between inflorescence 
        types


To run script in rb < infl_type_ard_binary.Rev

 ii. run code to combine runs
            awk 'FNR == 1 && NR != 1 { next } { print }' branchiness_run_1.txt branchiness_run_2.txt > branchiness_combined.txt
        iii.run code to remove first 10% of code
            total_lines=$(wc -l < branchiness_combined.txt)
            skip_lines=$((total_lines / 10))
            awk -v skip="$skip_lines" 'NR > skip || NR == 1' branchiness_combined.txt > branchiness_combined_trimmed.txt
