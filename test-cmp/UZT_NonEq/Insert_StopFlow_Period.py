# A script for inserting the stop flow period into the ftl file of the non-equilibrium test problem.
# The analytical solution was published by Vanderborght et al. (2005) "A set of analytical benchmarks
# to test numerical models of flow and transport in soils"  The numerical flow solution will still
# calculate gravity drainage even though water is no longer being inserted into the top of the column.
# To prevent this, this script manually overrides the flux terms of the stop flow period to 0.00 and
# during this time the dissolved and sorbed phases are equilibrating.

class switch(object):
    def __init__(self, value):
        self.value = value
        self.fall = False
    
    def __iter__(self):
        """Return the match method once, then stop"""
        yield self.match
        raise StopIteration
    
    def match(self, *args):
        """Indicate whether or not to enter a case suite"""
        if self.fall or not args:
            return True
        elif self.value in args: # changed for v1.5, see below
            self.fall = True
            return True
        else:
            return False

# Read until the beginning of the stop flow period is found
srch_str = '           4        '

sr = open('NonEq.ftl','r')
sw = open('NonEq_wStopQ.ftl','w')

# Start transferring lines until the srch_str is found
for line in sr:
    if len(line) > 16:
        if srch_str not in line[0:20]:
            sw.write(line)
        elif srch_str in line[0:20]:

            # transfer the current line to the new file
            sw.write(line)

            # Peel out next line for looking up in switch
            line1 = sr.next()
            sw.write(line1)

            # Use switch command to figure out what to do next
            for case in switch(line1):
                if case(" 'QXX             '\n"):
                    for i in xrange(126):
                        line1 = sr.next()
                        sw.write('  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00\n')

                    break
                if case(" 'QZZ             '\n"):
                    for i in xrange(126):
                        line1 = sr.next()
                        sw.write('  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00\n')

                    break
                if case(" 'STO             '\n"):
                    for i in xrange(126):
                        line1 = sr.next()
                        sw.write('  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00\n')

                    break
                if case(" 'CNH             '           5\n"):
                    for i in xrange(5):
                        line1 = sr.next()
                    sw.write('         209           1           1  0.0000000E+00\n')
                    sw.write('         209           1           3  0.0000000E+00\n')
                    sw.write('         210           1           1  0.0000000E+00\n')
                    sw.write('         210           1           2  0.0000000E+00\n')
                    sw.write('         210           1           3  0.0000000E+00\n')

                    break
                if case(" 'UZ FLUX         '\n"):
                    for i in xrange(126):
                        line1 = sr.next()
                        sw.write('  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00\n')

                    break
                if case(" 'UZQSTO          '\n"):
                    for i in xrange(126):
                        line1 = sr.next()
                        sw.write('  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00\n')

                    break
                if case():  # default
                    line1 = sr.next()
                    sw.write(line1)
                    break

    else:
        sw.write(line)
        
    

sr.close()
sw.close()
