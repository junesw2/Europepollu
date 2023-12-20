###############################################################################
###
### Population exposure to multiple air pollutants and its compound episodes in Europe
### December 2023
### Zhaoyue Chen
### 
################################################################################

# Import necessary libraries
import pyreadr
import pandas as pd
import patchworklib as pw
pw.overwrite_axisgrid() # To be executed when using pw.load_seaborngrid

import os
import numpy as np
import numpy.ma as ma
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import spearmanr

# Function to create a scatter plot with statistics
def makesweetgraph(x=None, y=None, corr=None, nmb=None, nrmse=None, max_lim=None, cmap='mako', ylab=None, xlab=None, bins=75, figsize=(5, 5), snsbins=100):
    ax1 = sns.jointplot(x=x, y=y, marginal_kws=dict(bins=snsbins), xlim=(0, max_lim), ylim=(0, max_lim))
    ax1.ax_joint.cla()
    plt.sca(ax1.ax_joint)
    plt.hist2d(x, y, bins, range=[[0, max_lim],[0, max_lim]], cmap=cmap, cmin=75)
    plt.ylabel(ylab, fontsize=12)
    plt.xlabel(xlab, fontsize=12)
    plt.plot([0, max_lim], [0, max_lim], ls='--', c='r')  # Add reference line y=x
    plt.text(0.05, 0.95, 'Cor: {:.2f}'.format(corr), transform=ax1.ax_joint.transAxes)
    plt.text(0.05, 0.90, 'NMB: {:.2f}'.format(nmb), transform=ax1.ax_joint.transAxes)
    plt.text(0.05, 0.85, 'NRMSE: {:.2f}'.format(nrmse), transform=ax1.ax_joint.transAxes)
    return ax1

# Function to plot air pollutant cross-validation data and statistics
    """
    Plots a scatter plot of the PM1 and PM1 F25 data along with the correlation coefficient, 
    normalized mean bias, and normalized root mean square error.

    Parameters:
    pm1_data (pandas.DataFrame): The PM1 data as a pandas DataFrame.
    pm1_col (str): The name of the PM1 column.
    pm1_f25_col (str): The name of the PM1 F25 column.
	snsbins : the numbers of bins for hist2d
    """
def plot_pm1(pm1_data, pm1_col, pm1_f25_col, snsbins=100):
    valid_mask = ~np.isnan(pm1_data[pm1_col]) & ~np.isnan(pm1_data[pm1_f25_col])
	corr = ma.corrcoef(ma.masked_array(pm1_data[pm1_col], mask=~valid_mask), 
                       ma.masked_array(pm1_data[pm1_f25_col], mask=~valid_mask))[0, 1]
    #corr, _ = spearmanr(pm1_data[pm1_col][valid_mask], pm1_data[pm1_f25_col][valid_mask])
    nmb = np.mean(pm1_data[pm1_f25_col] - pm1_data[pm1_col]) / np.mean(pm1_data[pm1_col]) * 100
    nrmse = np.sqrt(np.mean((pm1_data[pm1_f25_col] - pm1_data[pm1_col])**2)) / (np.percentile(pm1_data[pm1_col], 75) - np.percentile(pm1_data[pm1_col], 25)) * 100
    data = pm1_data.copy()
    max_lim = max(np.percentile(data[pm1_col], 99.5), np.percentile(data[pm1_f25_col], 99.5))
    sns.set_style("white")
    g = makesweetgraph(x=pm1_data[pm1_f25_col], y=pm1_data[pm1_col], corr=corr, nmb=nmb, nrmse=nrmse, max_lim=max_lim, cmap='jet', ylab=" ", xlab=" ", bins=75, snsbins=snsbins)
    return g

# Reading and plotting different data sets
var0 = "no2"
a2 = pyreadr.read_r("/Yourfolder/inputdata/" + var0 + "_validation.Rdata")
plot_df = a2[None]
g3 = pw.load_seaborngrid(plot_pm1(plot_df, var0, "spatpred"), figsize=(5, 5))

var0 = "o3_8h"
a2 = pyreadr.read_r("/Yourfolder/inputdata/" + var0 + "_validation.Rdata")
plot_df = a2[None]
g4 = pw.load_seaborngrid(plot_pm1(plot_df, var0, "spatpred"), figsize=(5, 5))

var0 = "pm10"
a2 = pyreadr.read_r("/Yourfolder/inputdata/" + var0 + "validation.Rdata")
plot_df = a2[None]
g2 = pw.load_seaborngrid(plot_pm1(plot_df, var0, "spatpred"), figsize=(5, 5))

var0 = "pm25"
a2 = pyreadr.read_r("/Yourfolder/inputdata/" + var0 + "validation.Rdata")
plot_df = a2[None]
g1 = pw.load_seaborngrid(plot_pm1(plot_df, var0, "spatpred"), figsize=(5, 5))

# Setting labels and titles for subplots
g1.set_supxlabel("Pred")
g1.set_supylabel("Obs")
g1.set_suptitle("a) PM2.5")
g2.set_supxlabel("Pred")
g2.set_supylabel("Obs")
g2.set_suptitle("b) PM10")
g3.set_supxlabel("Pred")
g3.set_supylabel("Obs")
g3.set_suptitle("c) NO2")
g4.set_supxlabel("Pred")
g4.set_supylabel("Obs")
g4.set_suptitle("d) O3_8h")

# Saving the plot to a file
folder = '/Yourfolder/docs/scriptforplot/figure/'
if not os.path.exists(folder):
    os.makedirs(folder)
ax1234 = (g1.outline | g2.outline) / (g3.outline | g4.outline)
ax1234.savefig(os.path.join(folder, 'Figure1.pdf'))


###############################################################################
###
### Population exposure to multiple air pollutants and its compound episodes in Europe
### December 2023
### Zhaoyue Chen
### 
################################################################################