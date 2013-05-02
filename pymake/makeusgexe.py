import os
import subprocess
import shutil


fc = 'ifort.exe'
srcdir = '../src'
target = 'mfusg'
platform = 'ia32'
#platform = 'ia32_intel64'
#platform = 'intel64'
if platform == 'ia32_intel64' or platform == 'intel64':
    target += '_x64'
compileflags = [
                '-O2',
                '-heap-arrays:0',
                '-fpe:0',
                '-traceback',
               ]
cpvars = os.environ.get('IFORT_COMPILER13') + 'bin/compilervars.bat'
#cpvars = 'C:/Program Files (x86)/Intel/Composer XE 2013/bin/compilervars.bat'
makeclean = True
srcfiles = ['gmodules.f',
             'GLO2btnU1.f', 
             'gwf2chd7U1.f', 
             'gwf2drn7U1.f', 
             'gwf2fhb7U1.f', 
             'gwf2ghb7U1.f', 
             'gwf2hfb7U1.f', 
             'gwf2riv7U1.f', 
             'gwf2rch8U1.f', 
             'gwf2evt8U1.f', 
             'Lak_Gag_Sfr_Modules.f', 
             'gwf2sfr7U1.f', 
             'gwf2str7U1.f', 
             'gwf2lak7U1.f', 
             'gwf2sub7U1.f', 
             'gwf2wel7U1.f', 
             'gwf2gag7U1.f', 
             'CLN2Props1.f', 
             'gwf2basU1.f', 
             'GWF2BCF-LPF-U1.f', 
             'xmdlib_2.f', 
             'DISU2GNCb1.f', 
             'DISU2GNCn1.f', 
             'xmd.f', 
             'parutl7.f', 
             'pcgu7.f', 
             'utl7U1.f', 
             'GLO2SMS-U1.f', 
             'glo2basU1.f', 
             'mfusg.f', 
             'CLN2BasU1.f' 
             ]

#create a batch file for compiling
f = open('compileusg.bat', 'w')
line = 'call ' + '"' + os.path.normpath(cpvars) + '" ' + platform + '\n'
f.write(line)

#build object files
for srcfile in srcfiles:
    cmd = fc + ' '
    for switch in compileflags:
        cmd += switch + ' '
    cmd += '-c' + ' '
    cmd += os.path.join(os.path.normpath(srcdir), srcfile)
    cmd += '\n'
    f.write(cmd)

#build executable
cmd = fc + ' '
for switch in compileflags:
    cmd += switch + ' '
cmd += '-o' + ' ' + target + ' ' + '*.obj' + '\n'
f.write(cmd)
f.close()

#run the batch file
subprocess.check_call(['compileusg.bat', ],)

#clean things up
if makeclean:
    print 'making clean...'
    filelist = os.listdir('.')
    delext = ['.mod', '.obj']
    for f in filelist:
        for ext in delext:
            if f.endswith(ext):
                os.remove(f)

print 'Done...'
    
    