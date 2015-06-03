import numpy as np
import matplotlib.pyplot as plt

def ed_trans_prob(*args):

     # Input Variables:
    # PS = prob_succession : probability of succession per timestep
    # SL = stage_length : maximum time spent in stage :
    #      equal to "after X years" (consider changing text to "by X years"
    # SY = start_year : year in stage at which probability of succession
    #      is implemented : subtract 10 from "begin transition after X years"
    tau = 10
    # AS : age range during which succession can take place
    for label, PS, SY, SL in args:
        AS = np.arange(0, SL + 3*tau, tau)
        if PS < 1:
           transition_prob = (1 - PS) ** ((AS - SY) / tau)
        elif PS == 1:
            transition_prob = np.zeros_like(AS)
        transition_prob[AS <= SY] = 1
        transition_prob[AS >= SL] = 0
        plt.plot(AS, transition_prob, label=label)
    plt.title("Transition Probability for SCN, Early Development")
    plt.xlabel("Years in Stage")
    plt.ylabel("Probability of patch remaining in Early Development")
    plt.legend()

def mdc_trans_prob(*args):
     # Input Variables:
    # PS = prob_succession : probability of succession per timestep
    # SL = stage_length : maximum time spent in stage :
    #      equal to "after X years" (consider changing text to "by X years"
    # SY = start_year : year in stage at which probability of succession
    #      is implemented : subtract 10 from "begin transition after X years"
    tau = 10
    # AS : age range during which succession can take place
    for label, PS, SY, SL in args:
        AS = np.arange(0, SL + 3*tau, tau)
        if PS < 1:
           transition_prob = (1 - PS) ** ((AS - SY) / tau)
        elif PS == 1:
            transition_prob = np.zeros_like(AS)
        transition_prob[AS <= SY] = 1
        transition_prob[AS >= SL] = 0
        plt.plot(AS, transition_prob, label=label)
    plt.title("Transition Probability for SCN, Mid Development Closed")
    plt.xlabel("Years in Stage")
    plt.ylabel("Probability of patch remaining in Mid Development Closed")
    plt.legend()

def mdo_trans_prob(*args):

     # Input Variables:
    # PS = prob_succession : probability of succession per timestep
    # SL = stage_length : maximum time spent in stage :
    #      equal to "after X years" (consider changing text to "by X years"  
    # SY = start_year : year in stage at which probability of succession
    #      is implemented : subtract 10 from "begin transition after X years"
    tau = 10
    # AS : age range during which succession can take place
    for label, PS, SY, SL in args:
        AS = np.arange(0, SL + 3*tau, tau)
        if PS < 1:
           transition_prob = (1 - PS) ** ((AS - SY) / tau)
        elif PS == 1:
            transition_prob = np.zeros_like(AS)
        transition_prob[AS <= SY] = 1
        transition_prob[AS >= SL] = 0
        plt.plot(AS, transition_prob, label=label)
    plt.title("Transition Probability for SCN, Mid Development Open")
    plt.xlabel("Years in Stage")
    plt.ylabel("Probability of patch remaining in Mid Development Open")
    plt.legend()

def ldo_trans_prob(*args):

     # Input Variables:
    # PS = prob_succession : probability of succession per timestep
    # SL = stage_length : maximum time spent in stage :
    #      equal to "after X years" (consider changing text to "by X years"
    # SY = start_year : year in stage at which probability of succession
    #      is implemented : subtract 10 from "begin transition after X years"
    tau = 10
    # AS : age range during which succession can take place
    for label, PS, SY, SL in args:
        AS = np.arange(0, SL + 3*tau, tau)
        if PS < 1:
           transition_prob = (1 - PS) ** ((AS - SY) / tau)
        elif PS == 1:
            transition_prob = np.zeros_like(AS)
        transition_prob[AS <= SY] = 1
        transition_prob[AS >= SL] = 0
        plt.plot(AS, transition_prob, label=label)
    plt.title("Transition Probability for SCN, Late Development Open")
    plt.xlabel("Years in Stage")
    plt.ylabel("Probability of patch remaining in Late Development Open")
    plt.legend()

def ldc_trans_prob(*args):

     # Input Variables:
    # PS = prob_succession : probability of succession per timestep
    # SL = stage_length : maximum time spent in stage :
    #      equal to "after X years" (consider changing text to "by X years"  
    # SY = start_year : year in stage at which probability of succession
    #      is implemented : subtract 10 from "begin transition after X years"
    tau = 10
    # AS : age range during which succession can take place
    for label, PS, SY, SL in args:
        AS = np.arange(0, SL + 3*tau, tau)
        if PS < 1:
           transition_prob = (1 - PS) ** ((AS - SY) / tau)
        elif PS == 1:
            transition_prob = np.zeros_like(AS)
        transition_prob[AS <= SY] = 1
        transition_prob[AS >= SL] = 0
        plt.plot(AS, transition_prob, label=label)
    plt.title("Transition Probability for SCN, Late Development Closed")
    plt.xlabel("Years in Stage")
    plt.ylabel("Probability of patch remaining in Late Development Closed")
    plt.legend()
