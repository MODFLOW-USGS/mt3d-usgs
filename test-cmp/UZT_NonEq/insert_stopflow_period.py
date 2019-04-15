'''A script for inserting the stop flow period into the ftl file of the
non-equilibrium test problem.

The analytical solution was published by Vanderborght et al. (2005) "A set of
analytical benchmarks to test numerical models of flow and transport in soils"
The numerical flow solution will still calculate gravity drainage even though
water is no longer being inserted into the top of the column. To prevent this,
this script manually overrides the flux terms of the stop flow period to 0.00
and during this time the dissolved and sorbed phases are equilibrating.
'''
import os


# Define what follows as a function so it is callable by another python script,
# in this case t003_test.py on the MT3D-USGS repo
def InsStpFlw(wdir):
    # Read until the beginning of the stop flow period is found
    # Start transferring lines until the srch_str is found
    srch_str = '           4        '
    with open(os.path.join(wdir, 'NonEq.ftl'), 'r') as sr:
        with open(os.path.join(wdir, 'NonEq_wStopQ.ftl'), 'w') as sw:
            while True:
                line = sr.readline()
                if not line:
                    break
                elif srch_str in line[0:20]:
                    sw.write(line)
                    # Peel out next line for looking up in switch
                    line = sr.readline()
                    sw.write(line)
                    label = line.strip().strip("'").strip()
                    if label in ('QXX', 'QZZ', 'STO', 'UZ FLUX', 'UZQSTO'):
                        num_items = 0
                        while num_items < 630:
                            items = sr.readline().split()
                            sw.write(len(items) * ' 0.0' + '\n')
                            num_items += len(items)
                        assert num_items == 630, num_items
                    elif label.startswith('CNH'):
                        for i in range(5):
                            items = sr.readline().split()
                            assert len(items) == 4, (label, items)
                        sw.write(' 209 1 1 0.0\n')
                        sw.write(' 209 1 3 0.0\n')
                        sw.write(' 210 1 1 0.0\n')
                        sw.write(' 210 1 2 0.0\n')
                        sw.write(' 210 1 3 0.0\n')
                    else:  # other datasets
                        sw.write(sr.readline())
                else:
                    sw.write(line)


if __name__ == '__main__':
    # executed as script in current directory to process NonEq.ftl
    InsStpFlw('.')
