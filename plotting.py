import matplotlib.pyplot as plt 
import numpy as np
import math

### RAW DATA
# Stong scaling for 1e6 blocks
nProc = np.array([1, 2, 4, 8, 12, 16, 20, 24])
timeS = np.array([51.109467, 31.853334, 22.032074, 22.815350, 21.126116, 21.647747, 21.309876, 21.221070])

# Weak scaling
nBlocks = np.arange(1000, 24000, 2000) 
timeW = np.array([1.064687, 1.359050, 1.682082, 1.132619, 2.165595, 2.190639, 2.574673, 2.619541, 3.171409, 3.506376, 3.259440, 3.446682])

# Optimal partition ratio testing
nPartitions_12c = np.array([4, 8, 12, 18, 24, 36, 48, 96])
timePartitions_12c = np.array([344, 278, 357, 319, 337, 325, 338, 337])

nPartitions_16c = np.array([4, 8, 12, 18, 24, 36, 48, 96])
timePartitions_16c = np.array([340, 264, 359, 432, 362, 417, 413, 423])

# Optimal repartition frequency (higher number means less frequent repartitioning)
nFrequency_12c = np.array([1, 2, 5, 10, 15, 20, 30, 50, 75, 200])
timeFrequency_12c = np.array([327, 259, 285, 283, 278, 381, 289, 517, 293, 740])

nFrequency_16c = np.array([1, 2, 5, 10, 15, 20, 30, 50, 75, 200])
timeFrequency_16c = np.array([318, 311, 288, 271, 264, 369, 290, 545, 306, 777])

### PROCESSED DATA
strong = np.zeros(nProc.size)
strongTime = np.zeros(nProc.size)
weak = np.zeros(nBlocks.size)

for i in range(0, len(strong)):
    strong[i] = (timeS[0]/(nProc[i]*timeS[i]))*100.0

for i in range(0, len(strongTime)):
    strongTime[i] = (timeS[0]/(timeS[i]))*100.0

for i in range(0, len(weak)):
    weak[i] = (timeW[0]/timeW[i])*100.0

fig1 = plt.figure()
ax1 = fig1.add_subplot(111)
ax1 = plt.axes(xlim=(1, 24), ylim=(10, 100))
line1, = ax1.plot(nProc[:], strong[:],linewidth=2, color='b', marker='o', label='Single machine: Intel Xeon E5 & 32GB RAM')
plt.title("Strong Scaling - 1 Million Blocks")
plt.ylabel('Scaling Efficiency (% of Linear Scaling)')
plt.xlabel('# of cores')
plt.legend(handles=[line1], loc=1)
plt.grid(b=True, which='major', linestyle='--')
plt.grid(b=True, which='minor', linestyle='--') 

fig2 = plt.figure()
ax2 = fig2.add_subplot(111)
# ax2 = plt.axes(xlim=(1, 24), ylim=(10, 100))
line1, = ax2.plot(nBlocks[:], weak[:],linewidth=2, color='b', marker='o', label='Single machine: Intel Xeon E5 & 32GB RAM')
plt.title("Weak Scaling - 1000 Blocks Per Core")
plt.ylabel('Scaling Efficiency (% of Linear Scaling)')
plt.xlabel('Number of Blocks')
plt.legend(handles=[line1], loc=1)
plt.grid(b=True, which='major', linestyle='--')
plt.grid(b=True, which='minor', linestyle='--') 

fig3 = plt.figure()
ax3 = fig3.add_subplot(111)
# ax3 = plt.axes(xlim=(1, 24), ylim=(10, 100))
line1, = ax3.plot(nPartitions_12c[:], timePartitions_12c[:],linewidth=2, color='b', marker='o', label='12 cores')
line2, = ax3.plot(nPartitions_16c[:], timePartitions_16c[:],linewidth=2, color='g', marker='o', label='16 cores')
plt.title("Partition Count Sensitivity - Single Machine Intel Xeon E5 with 32GB RAM")
plt.ylabel('Time (seconds)')
plt.xlabel('Number or partitions')
plt.legend(handles=[line1, line2], loc=1)
plt.grid(b=True, which='major', linestyle='--')
plt.grid(b=True, which='minor', linestyle='--') 

fig4 = plt.figure()
ax4 = fig4.add_subplot(111)
# ax4 = plt.axes(xlim=(1, 24), ylim=(10, 100))
line1, = ax4.plot(nFrequency_12c[:], timeFrequency_12c[:], linewidth=2, color='b', marker='o', label='12 cores')
line2, = ax4.plot(nFrequency_16c[:], timeFrequency_16c[:], linewidth=2, color='g', marker='o', label='16 cores')
plt.title("Repartition Frequency Sensitivity - Single Machine Intel Xeon E5 with 32GB RAM")
plt.ylabel('Time (seconds)')
plt.xlabel('Repartition Period (Discontinuity)')
plt.legend(handles=[line1, line2], loc=1)
plt.grid(b=True, which='major', linestyle='--')
plt.grid(b=True, which='minor', linestyle='--') 
plt.show()
