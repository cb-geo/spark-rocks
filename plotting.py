import matplotlib.pyplot as plt 
import numpy as np
import math

# Stong scaling for 1e6 blocks
nProc = np.array([1, 2, 4, 8, 12, 16, 20, 24])
timeS = np.array([51.109467, 31.853334, 22.032074, 22.815350, 21.126116, 21.647747, 21.309876, 21.221070])

# Weak scaling
nBlocks = np.arange(1000, 24000, 2000) 
timeW = np.array([1.064687, 1.359050, 1.682082, 1.132619, 2.165595, 2.190639, 2.574673, 2.619541, 3.171409, 3.506376, 3.259440, 3.446682])

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
line1, = ax1.plot(nProc[:], strong[:],linewidth=4, color='g', marker='o', label='Single machine: Intel Xeon E5 & 32GB RAM')
plt.title("Strong Scaling - 1 Million Blocks", fontsize=20)
plt.ylabel('Scaling Efficiency (% of Linear Scaling)', fontsize=16)
plt.xlabel('# of cores', fontsize=16)
plt.legend(handles=[line1], loc=1,fontsize=16)
plt.grid(b=True, which='major', linestyle='-')
plt.grid(b=True, which='minor', linestyle='--') 

fig2 = plt.figure()
ax2 = fig2.add_subplot(111)
# ax2 = plt.axes(xlim=(1, 24), ylim=(10, 100))
line1, = ax2.plot(nBlocks[:], weak[:],linewidth=4, color='b', marker='o', label='Single machine: Intel Xeon E5 & 32GB RAM')
plt.title("Weak Scaling - 1000 Blocks Per Core", fontsize=20)
plt.ylabel('Scaling Efficiency (% of Linear Scaling)', fontsize=16)
plt.xlabel('Number of Blocks', fontsize=16)
plt.legend(handles=[line1], loc=1, fontsize=16)
plt.grid(b=True, which='major', linestyle='-')
plt.grid(b=True, which='minor', linestyle='--') 
plt.show()
# fig3 = plt.figure()
# ax3 = fig3.add_subplot(111)
# # ax3 = plt.axes(xlim=(1, 24), ylim=(10, 100))
# line1, = ax3.plot(nBlocks[:], timeW[:],linewidth=2, color='b', marker='o', label='Single machine: Intel Xeon E5 & 32GB RAM')
# plt.title("Weak Scaling")
# plt.ylabel('Time (seconds)')
# plt.xlabel('Number of Blocks')
# plt.legend(handles=[line1], loc=1)
# plt.grid(b=True, which='major', linestyle='-')
# plt.grid(b=True, which='minor', linestyle='--') 

# fig4 = plt.figure()
# ax4 = fig4.add_subplot(111)
# # ax4 = plt.axes(xlim=(1, 24), ylim=(10, 100))
# line1, = ax4.plot(nProc[:], strongTime[:],linewidth=2, color='g', marker='o', label='Single machine: Intel Xeon E5 & 32GB RAM')
# plt.title("Strong Scaling")
# plt.ylabel('Time(single processor)/Time(N processors)')
# plt.xlabel('# of cores')
# plt.legend(handles=[line1], loc=1)
# plt.grid(b=True, which='major', linestyle='-')
# plt.grid(b=True, which='minor', linestyle='--') 
# plt.show()
