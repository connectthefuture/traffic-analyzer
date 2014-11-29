import csv
import getopt
import sys


def clean_any_data(source_file_name, dest_file_name, row_test, header=True):
    """
    This will clean each row of a CSV file. If the row passes test
    then it will be added to the output file. Otherwise it will be discarded.
    """

    cleaned_data = []
    with open(source_file_name, 'r') as csvfile:
        data = csv.reader(csvfile, delimiter=',')
        # If we have a header it probably won't pass the test so just read
        # it.
        if header:
            cleaned_data.append(next(data, None))
        for row in data:
            if row_test(row):
                cleaned_data.append(row)

    with open(dest_file_name, 'w') as outfile:
        wr = csv.writer(outfile, delimiter=',')
        wr.writerows(cleaned_data)

def region_test(row):
    """
    Says if a specific row is valid.
    """

    return row[-2] != "0" and float(row[-1]) < 40 and float(row[-1]) > 0

def segment_test(row):
    """
    Says if a specific row is valid.
    """

    return row[-1] != "-1"

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print "Usage: cleanup_congestion.py [infile] [outfile] [-s]"
        sys.exit(-1)

    try:
        opts, args = getopt.getopt(sys.argv, "s", ["segment"])
    except getopt.GetoptError:
        sys.exit(-1)

    test = region_test
    for opt, _ in opts:
        if opt in ('-s', '--segment'):
            test = segment_test


    clean_any_data(sys.argv[1], sys.argv[2], test)
