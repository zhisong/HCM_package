import os
import numpy as np
import matplotlib.pyplot as plt
import f90nml

class MishkaF:
    def __init__(self, mishka_path):
      """Set the MISHKA running path
      """
      self._mishka_path = mishka_path
      # check the existence
      if not os.path.exists(mishka_path):
        raise FileNotFoundError('The specified HELENA path cannot be found')

      self._namelist_dir = os.path.join(mishka_path, 'namelist')
      self._output_dir = os.path.join(mishka_path, 'output')
      self._mappint_dir = os.path.join(mishka_path, 'mapping')
      self._runmis_dir = os.path.join(mishka_path, 'runmis')

    def set_equilibrium(self, helena_eq):
      """Connect to a helena equilibrium"""
      self._helena_eq = helena_eq
      self._hel_map_path = os.path.join(helena_eq._mappint_dir, helena_eq._runname)
      self._hel_map_name = helena_eq._runname

    def read_namelist(self, namelist_name):
      """Read a namelist of MISHKA"""
      namelist_fullpath = os.path.join(self._namelist_dir, namelist_name)
      # Read in the file
      with open(namelist_fullpath, 'r') as file:
        filedata = file.read()

      # Replace the target string
      filedata = filedata.replace('&END', '/')

      self._namelist_name = namelist_name
      self._namelist_fullpath = namelist_fullpath

      # Write the file out again
      with open(namelist_fullpath, 'w') as file:
        file.write(filedata)

      self.namelist = f90nml.read(namelist_fullpath)

    def print_namelist(self):
        print(self.namelist)

    def run(self, namelist_name = None):
      import subprocess
      import shutil
      if namelist_name is None:
        if self.namelist is None:
          raise FileNotFoundError("Please specify namelist file")
        else:
          print("Will run namelist " + self._namelist_name)
      else:
        self.read_namelist(namelist_name)

      self._runname = self._namelist_name + 'py'
      namelistrun_fullpath = os.path.join(self._namelist_dir, self._runname)
      self.namelist.write(namelistrun_fullpath, force=True)
      os.chdir(self._mishka_path)
      src = self._hel_map_path
      dst = os.path.join(self._mappint_dir, self._hel_map_name)
      shutil.copy(src, dst)
      subprocess.run(["./runmis", self._hel_map_name, self._runname], capture_output=True)
      self.read_output(self._runname)
      print(r'lambda=', str(self._lambda))

    def read_output(self, filename):
      """Read MISHKA output"""
      filename_fullpath = os.path.join(self._output_dir, filename)
      filename22_fullpath = os.path.join(self._output_dir, filename+'.22')

      # look for bussac beta
      search_string = "BUSSAC BETA AT Q=1 : S="
      found_bussac = False
      with open(filename_fullpath, 'r') as file:
          for line_number, line in enumerate(file, start=1):
              if search_string in line:
                  line = line.replace(search_string, "")
                  line = line.replace("BETA(B) =", "")
                  sloc, betabussac = map(float, line.split())
                  found_bussac = True
                  break
      if found_bussac == True:
        self._bussac_beta = betabussac
        self._q1surface = sloc
        print(r'q=1 at s=' + str(sloc) + ' Bussac Beta=' + str(betabussac))

      with open(filename22_fullpath, 'r') as f:
        # the first line, reads the frequency
        line = f.readline()
        lambdare, lambdaim = line.split()
        self._lambda = float(lambdare) + 1j * float(lambdaim)

        # the second line, reads the number of radial points and the number of modes
        line = f.readline()
        nr, nmode = map(int, line.split())
        self._nr_result = nr
        self._nmode_result = nmode

        mlist = []
        while len(mlist) < nmode:
          line = f.readline()
          item = list(map(float, line.split()))
          mlist = mlist + item

        self._mlist = np.int32(np.array(mlist))

        rlist = []
        while len(rlist) < nr:
          line = f.readline()
          item = list(map(float, line.split()))
          rlist = rlist + item

        self._rlist = np.array(rlist)

        evlist = []
        while len(evlist) < 2 * 7 * 2 * nmode * nr:
          line = f.readline()
          item = list(map(float, line.split()))
          evlist = evlist + item

        self._evlist = np.array(evlist).reshape([nr,7,nmode,2,2])

    def plot_vs(self):
      plt.figure()
      for i in range(self._mlist.size):
        plt.plot(self._rlist[1:], self._evlist[1:,0,i,0,0]/self._rlist[1:], label='m='+str(self._mlist[i]))

      plt.xlabel(r'$s$')
      plt.ylabel(r'$Re(V_s)$ (A.U.)')
      plt.legend()
      plt.title(r'Real part of $V_s$')

      plt.figure()
      for i in range(self._mlist.size):
        plt.plot(self._rlist[1:], self._evlist[1:,0,i,0,1]/self._rlist[1:], label='m='+str(self._mlist[i]))

      plt.xlabel(r'$s$')
      plt.ylabel(r'$Im(V_s) (A.U.)$')
      plt.legend()
      plt.title(r'Imaginary part of $V_s$')
