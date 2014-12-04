import csv
import sys
from datetime import datetime


def read_sports_games(sports_game_file):
    """
    This will read in a sports games file and produce a dictionary that maps
    from a date to a tuple of the sports games that happened on that day. It
    will also return the header of the CSV file.
    """

    sports_games = {}
    with open(sports_game_file, 'r') as csvfile:
        data = csv.reader(csvfile, delimiter=',')
        header = next(data, None)
        for row in data:
            date = datetime.strptime(row[0],"%Y-%m-%d").date()
            sports_games[date] = (row[1:] + [sum([int(x) for x in row[1:]])])
    return sports_games, header[1:] + ['any_game']

def augment_data(source_file_name, dest_file_name, sports_games):
    """
    This function will augment congestion data. It will convert the single
    datetime to many different variables, and adds the sports games.
    """

    sports_data, sports_header = read_sports_games(sports_games)

    augmented_data = []
    day_label = 1
    last_day = None
    with open(source_file_name, 'r') as csvfile:
        data = csv.reader(csvfile, delimiter=',')
        next(data, None) # Skip the headder
        for row in data:
            try:
                t = datetime.strptime(row[0], "%m/%d/%Y %I:%M:%S %p")
            #If for some reason we can't parse this just continue
            except ValueError:
                continue

            # Update the day label if we need to
            if last_day is None:
                last_day = t.date()
            elif last_day < t.date():
                last_day = t.date()
                day_label += 1

            # Check if this is a weekend
            if t.weekday() >= 5:
                weekend = 1
            else:
                weekend = 0

            vals = [row[1], day_label, row[-1],
                    t.year, t.month, t.day,
                    t.hour, t.minute, t.weekday(), weekend]
            vals += sports_data[t.date()]
            augmented_data.append(vals)

    with open(dest_file_name, 'w') as csvfile:
        wr = csv.writer(csvfile, delimiter=',')
        wr.writerow(['id', 'day_label', 'speed', 'year', 'month', 'day',
                     'hour', 'minute', 'weekday', 'weekend'] + sports_header)
        wr.writerows(augmented_data)

if __name__ == "__main__":
    if len(sys.argv) < 4:
        print "Usage: cleanup_congestion.py [infile] [outfile] [sports_file]"
        sys.exit(-1)

    augment_data(sys.argv[1], sys.argv[2], sys.argv[3])
