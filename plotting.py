import matplotlib.pyplot as plt 
import numpy as np
import math

### RAW DATA
# Stong scaling for 1e6 blocks
nProc = np.array([1, 2, 4, 8, 12, 16, 20, 24])
timeStrong_8 = np.array([1574, 956, 593, 451, 449, 461, 476, 473])
timeStrong_16 = np.array([1506, 932, 569, 476, 597, 651, 650, 689])
timeStrong_24 = np.array([1473, 881, 592, 480, 604, 611, 686, 726])

# Weak scaling
nBlocks = np.array([4096, 8000, 17576, 32768, 50653, 64000, 85184, 97336]) 
timeWeak = np.array([40, 59, 97, 201, 328, 475, 689, 982])
 
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
strong_8 = np.zeros(nProc.size)
strong_16 = np.zeros(nProc.size)
strong_24 = np.zeros(nProc.size)
# strongTime = np.zeros(nProc.size)
weak = np.zeros(nBlocks.size)

for i in range(0, len(strong_8)):    
    strong_8[i] = (timeStrong_8[0]/(float(nProc[i])*timeStrong_8[i]))*100.0
    strong_16[i] = (timeStrong_16[0]/(float(nProc[i])*timeStrong_16[i]))*100.0
    strong_24[i] = (timeStrong_24[0]/(float(nProc[i])*timeStrong_24[i]))*100.0

# for i in range(0, len(strongTime)):
#     strongTime[i] = (timeS[0]/(timeS[i]))*100.0

for i in range(0, len(weak)):
    weak[i] = (timeWeak[0]/float(timeWeak[i]))*100

fig1 = plt.figure()
ax1 = fig1.add_subplot(111)
# ax1 = plt.axes(xlim=(1, 24), ylim=(0, 100))
line1, = ax1.plot(nProc[:], strong_8[:],linewidth=2, color='b', marker='o', label='8 partitions')
line2, = ax1.plot(nProc[:], strong_16[:],linewidth=2, color='g', marker='o', label='16 partitions')
line3, = ax1.plot(nProc[:], strong_24[:],linewidth=2, color='r', marker='o', label='24 partitions')
plt.title("Strong Scaling - 64,000 Blocks - Single machine Intel Xeon E5 & 32GB RAM")
plt.ylabel('Scaling Efficiency (% of Linear Scaling)')
plt.xlabel('# of cores')
plt.legend(handles=[line1, line2, line3], loc=1)
plt.grid(b=True, which='major', linestyle='--')
plt.grid(b=True, which='minor', linestyle='--') 

fig2 = plt.figure()
ax2 = fig2.add_subplot(111)
# ax2 = plt.axes(xlim=(1, 24), ylim=(0, 100))
line1, = ax2.plot(nBlocks[:], weak[:],linewidth=2, color='b', marker='o', label='Partitions = 8, Repartition Period = 15 joints')
plt.title("Weak Scaling - 4000 Blocks Per Core - Single machine Intel Xeon E5 & 32GB RAM")
plt.ylabel('Scaling Efficiency (% of Linear Scaling)')
plt.xlabel('Number of Blocks')
plt.legend(handles=[line1], loc=1)
plt.grid(b=True, which='major', linestyle='--')
plt.grid(b=True, which='minor', linestyle='--') 

fig3 = plt.figure()
ax3 = fig3.add_subplot(111)
# ax3 = plt.axes(xlim=(1, 24), ylim=(0, 100))
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

fig5 = plt.figure()
ax5 = fig5.add_subplot(111)
# ax5 = plt.axes(xlim=(1, 24), ylim=(0, 100))
line1, = ax5.plot(nProc[:], timeStrong_8[:],linewidth=2, color='b', marker='o', label='8 partitions')
line2, = ax5.plot(nProc[:], timeStrong_16[:],linewidth=2, color='g', marker='o', label='16 partitions')
line3, = ax5.plot(nProc[:], timeStrong_24[:],linewidth=2, color='r', marker='o', label='24 partitions')
plt.title("Strong Scaling - 64,000 Blocks - Single machine Intel Xeon E5 & 32GB RAM")
plt.ylabel('Time (seconds)')
plt.xlabel('# of cores')
plt.legend(handles=[line1, line2, line3], loc=1)
plt.grid(b=True, which='major', linestyle='--')
plt.grid(b=True, which='minor', linestyle='--') 

fig6 = plt.figure()
ax6 = fig6.add_subplot(111)
# ax6 = plt.axes(xlim=(1, 24))
line1, = ax6.plot(nBlocks[:], timeWeak[:],linewidth=2, color='b', marker='o', label='Partitions = 8, Repartition Period = 15 joints')
plt.title("Weak Scaling - 4000 Blocks Per Core - Single machine Intel Xeon E5 & 32GB RAM")
plt.ylabel('Time (seconds)')
plt.xlabel('Number of Blocks')
plt.legend(handles=[line1], loc=1)
plt.grid(b=True, which='major', linestyle='--')
plt.grid(b=True, which='minor', linestyle='--') 
plt.show()

