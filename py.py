import pandas as pd
import numpy as np
import seaborn as sns 
import matplotlib.pyplot as plt
import os
import scipy.stats as stats
import statsmodels.stats.api as sms
import statsmodels.stats.descriptivestats as smsd

df = pd.read_excel(r"C:\\Users\\nherm\\Downloads\\QNM\\Data11.xlsx")
df.head()
print(df.shape)
df.info()
df.describe()

#Correlation matrice
corr_df = df.corr()
plt.figure(figsize=(8,8))
import seaborn as sns
sns.heatmap(corr_df,
           annot=True,
           cbar=True,
           cmap="RdYlGn",
           linewidths=0.10,
           fmt=".2f",
           annot_kws={'size':10}, vmin=-1, vmax=1);

#1.Confidence interval
mean_x = df['x1'].mean()                                                       # sample mean
s_x = df['x1'].std(ddof = 1)                                                   # sample standard deviation
n = len(df['x1'])                                                              # sample size

stats.t.interval(alpha = 0.99,                                                 # confidence level
                 df = n - 1,                                                   # degrees of freedom
                 loc = mean_x,                                                 # sample mean
                 scale = s_x / n ** 0.5)

import pandas as pd
df = pd.read_excel(r"C:\\Users\\nherm\\Downloads\\QNM\\Data12.2.xlsx")
df.head()
df.describe()

x1=df["x1"]
x2=df["x2"]
y= df["y"]

from mpl_toolkits import mplot3d
# Creating figure
fig = plt.figure(figsize = (8, 4))
ax = plt.axes(projection ="3d")
# Creating plot
ax.scatter3D(x1, y, x2, color = "#2d3436")
plt.title("simple 3D scatter plot") 
# show plot
plt.show()              

