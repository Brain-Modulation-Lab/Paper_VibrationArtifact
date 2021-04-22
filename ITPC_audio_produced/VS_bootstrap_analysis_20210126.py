# -*- coding: utf-8 -*-
"""
This is a script to check if specific brain regions have different coherence values using the hierarchical bootstrap technique.
"""

import numpy as np
import pandas as pd

data = pd.read_csv('/Volumes/Nexus/Commits/Vibration_artifacts/audio_p-coherence_syllable_triplet/data/coherence_audio_p_by_electrode_mean.tsv', sep='\t')
np.random.seed(31415)

#Define auxillary functions:

def get_bootstrapped_sample(variable,area='all',nboot=10000):
    
    #critical assumption in the way the bootstrap is currently run:
    #Every subject is weighted by the number of electrodes in a particular region that they have.
    #So a region that has a few subjects with a large number of electrodes will be correspondingly influenced by those subjects.
    
    #Initialize the output variable:
    bootstats = np.zeros(nboot)
    
    if area != 'all':
        variable = variable[variable['MMP1'] == area]
    
    for i in np.arange(nboot):
        #Use a temporary variable to collect all the values to be averaged:
        temp = np.empty((1,))
        subjects = pd.unique(variable['subject'])
        num_subs = np.shape(subjects)[0]
        rand_subs = np.random.choice(num_subs,num_subs)
        
        for j in rand_subs:
            subset = variable[variable['subject']==subjects[j]]
            num_samps = np.shape(subset)[0]
            rand_samps = np.random.choice(num_samps,num_samps)
            temp = np.append(temp, subset.iloc[rand_samps,9].to_numpy())
        
        #Remove 1st entry since that was not initialized:
        temp = temp[1:]
        #Collect the means from the entire set for each iteration:
        bootstats[i] = np.nanmean(temp)
        
    return bootstats

def get_direct_prob(sample1, sample2):
    '''
    get_direct_prob Returns the direct probability of items from sample2 being
    greater than or equal to those from sample1.
       Sample1 and Sample2 are two bootstrapped samples and this function
       directly computes the probability of items from sample 2 being greater
       than or equal to those from sample1. Since the bootstrapped samples are
       themselves posterior distributions, this is a way of computing a
       Bayesian probability. The joint matrix can also be returned to compute
       directly upon.
    '''
    joint_low_val = min([min(sample1),min(sample2)])
    joint_high_val = max([max(sample1),max(sample2)])
    
    p_joint_matrix = np.zeros((100,100))
    p_axis = np.linspace(joint_low_val,joint_high_val,num=100)
    edge_shift = (p_axis[2] - p_axis[1])/2
    p_axis_edges = p_axis - edge_shift
    p_axis_edges = np.append(p_axis_edges, (joint_high_val + edge_shift))

    #Calculate probabilities using histcounts for edges.

    p_sample1 = np.histogram(sample1,bins=p_axis_edges)[0]/np.size(sample1)
    p_sample2 = np.histogram(sample2,bins=p_axis_edges)[0]/np.size(sample2)

    #Now, calculate the joint probability matrix:

    for i in np.arange(np.shape(p_joint_matrix)[0]):
        for j in np.arange(np.shape(p_joint_matrix)[1]):
            p_joint_matrix[i,j] = p_sample1[i]*p_sample2[j]
            
    #Normalize the joint probability matrix:
    p_joint_matrix = p_joint_matrix/np.sum(p_joint_matrix)
    
    #Get the volume of the joint probability matrix in the upper triangle:
    p_test = np.sum(np.triu(p_joint_matrix))
    
    return p_test, p_joint_matrix

#Get null distribution:
bootstats_null = get_bootstrapped_sample(data)

#Compute the distribution for each region of interest and compare to the null:
#For reference, Alan's LMM results are below:

# Fixed effects:
#               Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  4.387e-01  3.362e-02  6.012e+01  13.049  < 2e-16 ***
# MMP143      -3.303e-03  2.881e-02  4.227e+03  -0.115  0.90873    
# MMP144      -2.844e-02  2.185e-02  4.234e+03  -1.302  0.19313    
# MMP155b     -6.817e-02  2.076e-02  4.223e+03  -3.283  0.00104 ** 
# MMP16r      -2.204e-02  3.061e-02  4.225e+03  -0.720  0.47149    
# MMP16v      -3.914e-02  1.582e-02  4.230e+03  -2.474  0.01340 *  
# MMP18Av     -9.660e-02  2.113e-02  4.230e+03  -4.572 4.98e-06 ***
# MMP18C      -9.633e-02  2.341e-02  4.232e+03  -4.115 3.94e-05 ***
# MMP1A4       1.010e-02  1.798e-02  4.238e+03   0.562  0.57429    
# MMP1A5      -3.107e-02  2.696e-02  4.234e+03  -1.153  0.24917    
# MMP1OP4      2.850e-02  2.137e-02  4.237e+03   1.334  0.18232    
# MMP1p9-46v  -8.065e-02  2.986e-02  4.229e+03  -2.701  0.00694 ** 
# MMP1PF       4.830e-02  2.103e-02  4.241e+03   2.297  0.02166 *  
# MMP1PFop     1.035e-02  1.887e-02  4.226e+03   0.549  0.58325    
# MMP1PSL     -9.227e-02  3.319e-02  4.230e+03  -2.780  0.00545 ** 
# MMP1STV     -7.000e-02  3.001e-02  4.235e+03  -2.332  0.01972 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Now, let's begin computing the bootstrapped sample for each area Alan wished to test above and compute the difference to the null:
#Area 43:
area = '43'
bootstats = get_bootstrapped_sample(data, area)
p_43 = get_direct_prob(bootstats_null, bootstats) #p_43 = 0.599

#Area 44:
area = '44'
bootstats = get_bootstrapped_sample(data, area)
p_44 = get_direct_prob(bootstats_null, bootstats) #p_44 = 0.517

#Area 55b:
area = '55b'
bootstats = get_bootstrapped_sample(data, area)
p_55b = get_direct_prob(bootstats_null, bootstats) #p_55b = 0.422

#Area 6r:
area = '6r'
bootstats = get_bootstrapped_sample(data, area)
p_6r = get_direct_prob(bootstats_null, bootstats) #p_6r = 0.817

#Area 6v:
area = '6v'
bootstats = get_bootstrapped_sample(data, area)
p_6v = get_direct_prob(bootstats_null, bootstats) #p_6v = 0.133

#Area 8Av:
area = '8Av'
bootstats = get_bootstrapped_sample(data, area)
p_8Av = get_direct_prob(bootstats_null, bootstats) #p_8Av = 0.218

#Area 8C:
area = '8C'
bootstats = get_bootstrapped_sample(data, area)
p_8C = get_direct_prob(bootstats_null, bootstats) #p_8C = 0.190

#Area A4:
area = 'A4'
bootstats = get_bootstrapped_sample(data, area)
p_A4 = get_direct_prob(bootstats_null, bootstats) #p_A4 = 0.568

#Area A5:
area = 'A5'
bootstats = get_bootstrapped_sample(data, area)
p_A5 = get_direct_prob(bootstats_null, bootstats) #p_A5 = 0.504

#Area OP4:
area = 'OP4'
bootstats = get_bootstrapped_sample(data, area)
p_OP4 = get_direct_prob(bootstats_null, bootstats) #p_OP4 = 0.923

#Area p9-46v:
area = 'p9-46v'
bootstats = get_bootstrapped_sample(data, area)
p_p9_46v = get_direct_prob(bootstats_null, bootstats) #p_p9_46v = 0.443

#Area PF:
area = 'PF'
bootstats = get_bootstrapped_sample(data, area)
p_PF = get_direct_prob(bootstats_null, bootstats) #p_PF = 0.990 * #Bootstats mean = 0.604

#AreaPFop:
area = 'PFop'
bootstats = get_bootstrapped_sample(data, area)
p_PFop = get_direct_prob(bootstats_null, bootstats) #p_PFop = 0.359

#Area PSL:
area = 'PSL'
bootstats = get_bootstrapped_sample(data, area)
p_PSL = get_direct_prob(bootstats_null, bootstats) #p_PSL = 0.222

#Area STV:
area = 'STV'
bootstats = get_bootstrapped_sample(data, area)
p_STV = get_direct_prob(bootstats_null, bootstats) #p_STV = 0.133

