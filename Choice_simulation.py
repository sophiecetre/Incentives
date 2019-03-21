#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 14 17:58:34 2019

@author: cetre

This script simulates choices that should take subjects depending on the 
current values of alpha (4 and 6), each piece rate wage contracts and the
three beta values obtained in the finite mixture model

Goal: having a way to simulate the choices of the intermediary class

So far: use best responses
Future steps: use effort beliefs

Second goal: this could help validate the results of the behavioral model.
If the utility function is correct, we should not see too many variations
from these predictions (once we take beliefs into account)
"""

import numpy
import pandas as pd
import os

os.chdir("/Users/cetre/Dropbox/Dossiers_partages/Incentives_shared/Theory_part/simulation")


# UNVARIANT PARAMETERS
# Alphas (ability parameters)
alpha_L = 4
alpha_H = 6

# Principal piece rate
P = 0.5

# Piece rate contracts (stored in a list of tuples in the order p_HA, p_LA, p_HB, p_LB)

piece_rates = [(0.4,0.6,0.5,0.5),(0.4,0.6,0.55,0.45),(0.4,0.6 ,0.6,0.4),
     (0.5,0.65,0.55,0.45),(0.5,0.65,0.6,0.4),(0.5,0.65,0.65,0.35),
     (0.5,0.65,0.7,0.3),(0.5,0.75,0.6,0.4),(0.5,0.75 ,0.65,0.35 ),
     (0.5,0.75,0.7,0.3),(0.5,0.5,0.55,0.45),(0.5,0.5 ,0.6,0.4),
     (0.55,0.55,0.5,0.65),(0.55,0.55,0.6,0.4),(0.55,0.55,0.65,0.35),
     (0.55,0.55,0.7,0.3)]
      

# Real cost function (in a dictionary where the key is effort and the value is the associated cost)
cost_function = {0  :0,
                 0.5:0.1,
				 1  :0.6, 
				 1.5:1.35,
				 2  :2.3,
				 2.5:3.45, 
				 3  :4.8, 
				 3.5:6.4,
				 4  :8.1, 
				 4.5:10.0, 
                 5  :12.0}

# Effort function
effort_function = numpy.arange(0, 5.5, 0.5)

# Function to determine workers best response
def best_response(p,alpha):
    BR = 0
    for e in effort_function:
        if p*alpha*e-cost_function[e] > p*alpha*BR-cost_function[BR]:
            BR =  e
        else:
            BR = BR
    return BR


# Inequality aversion parameter (as elicited by FMM)
FMM_beta_class = [0,0.6,1.44]


# simulation() simulates the choice for each contract
    
class simulation():
    
    def __init__(self,beta,p):
        # Piece rate for high and low alpha in contracts A and B
        self.p_HA = p[0]
        self.p_LA = p[1]
        self.p_HB = p[2]
        self.p_LB = p[3]  
        
        # Beta parameter (degree of inequality aversion)
        self.beta = beta
        
        # Best response effort depending on the piece rate in each contract and alpha
        self.e_HA = best_response(p[0],alpha_H)    
        self.e_LA = best_response(p[1],alpha_L)
        self.e_HB = best_response(p[2],alpha_H) 
        self.e_LB = best_response(p[3],alpha_L)

    
    # Build up cost based on the cost function 
    def cost(self):
        self.c_LA = cost_function[self.e_LA]
        self.c_HA = cost_function[self.e_HA]
        self.c_LB = cost_function[self.e_LB]
        self.c_HB = cost_function[self.e_HB]
        return self.c_LA,self.c_HA, self.c_LB, self.c_HB   
         
    # Compute utility functions with discretize effort and appropriate cost function
    def U(self):
        self.UA = P*(alpha_H*self.e_HA + alpha_L*self.e_LA) - self.beta*(self.p_HA*alpha_H*self.e_HA-self.c_HA-(self.p_LA*alpha_L*self.e_LA-self.c_LA))
        self.UB = P*(alpha_H*self.e_HB + alpha_L*self.e_LB) - self.beta*(self.p_HB*alpha_H*self.e_HB-self.c_HB-(self.p_LB*alpha_L*self.e_LB-self.c_LB)) 
        return self.UA, self.UB

    # Function that simulates the choice
    def choice(self):
        print "UA = {}".format(self.UA)
        print "UB = {}".format(self.UB)
        print "e_LA = {}".format(self.e_LA)
        print "e_HA = {}".format(self.e_HA) 
        print "e_LB = {}".format(self.e_LB)
        print "e_HB = {}".format(self.e_HB)            
        if self.UA > self.UB:
            self.choice = 'A'
            print("Should choose Contract A")
        elif self.UA == self.UB:
            self.choice = 'Indifferent'
            print("Should be indifferent")
        else:
            self.choice = 'B'
            print("Should choose Contract B")
        return self.choice

 


# Predict choices made for the 16 contract decisions
    
for i in range(0,16):
    contract_number = i +1
    print("Contract {}".format(contract_number))
    print "Piece rates", piece_rates[i]
    contract = simulation(FMM_beta_class[1],piece_rates[i])
    contract.cost()
    contract.U()    
    contract.choice()    
    print("----------------------")
    
    
    

# Sequence of betas
betas = numpy.arange(0,1.01,0.01).tolist()
print betas

# Make an array of choices (A or B) for the 16 contracts, for different beta values

bigd = []
for b in betas: 
    d = {}
    for i in range(0,16):  
        contract = simulation(b,piece_rates[i])
        contract.cost()
        contract.U() 
        d['Choice {}'.format(i+1)] = contract.choice()
        d['beta'] = b
    bigd.append(d) 

    
df = pd.DataFrame(bigd)

# Rearrange the order of the columns
cols = ['beta']
for i in range(0,16):
    cols.append('Choice {}'.format(i+1))

df = df[cols]

# Save to excel
df.to_excel('choice_simulation_by_betas.xlsx')

print cols
