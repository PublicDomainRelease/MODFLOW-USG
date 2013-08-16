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
             'sparse.f',
             'glo2btnu1.f', 
             'gwf2chd7u1.f', 
             'gwf2drn7u1.f', 
             'gwf2fhb7u1.f', 
             'gwf2ghb7u1.f', 
             'gwf2hfb7u1.f', 
             'gwf2riv7u1.f', 
             'gwf2rch8u1.f', 
             'gwf2evt8u1.f', 
             'lak_gag_sfr_modules.f', 
             'gwf2sfr7u1.f', 
             'gwf2str7u1.f', 
             'gwf2lak7u1.f', 
             'gwf2sub7u1.f', 
             'gwf2wel7u1.f', 
             'gwf2gag7u1.f', 
             'cln2props1.f', 
             'gwf2basu1.f', 
             'gwf2bcf-lpf-u1.f', 
             'xmdlib_2.f', 
             'disu2gncb1.f', 
             'disu2gncn1.f', 
             'xmd.f', 
             'parutl7.f', 
             'pcgu7.f', 
             'utl7u1.f', 
             'glo2sms-u1.f', 
             'glo2basu1.f', 
             'mfusg.f', 
             'cln2basu1.f' 
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
    
    