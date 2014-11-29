import csv
from datetime import datetime, timedelta


def get_basketball_games():
    """
    Returns a list of the home games with the day and a boolean to indicate
    win or loss.
    """

    with open('basketball.csv', 'r') as csvfile:
        reader = csv.reader(csvfile)
        home_games = []
        for line in reader:
            if line[-1] == 'at Chicago':
                game_date = datetime.strptime(line[0], "%b %d, %Y")
                won = (line[1] == "Chicago Bulls")
                home_games.append((game_date, won))
    return home_games

def get_baseball_games():
    """
    Returns two lists of (date, won) for all home games. The first is for
    the cubs and the second is for the whitesox
    """

    cubs = []
    with open('cubs.csv', 'r') as csvfile:
        reader = csv.reader(csvfile)
        for line in reader:
            try:
                current_year = int(line[0])
                continue
            except ValueError:
                pass

            if line[2] == "Postponed":
                continue

            game_date = datetime.strptime(str(current_year) + ' ' + line[0],
                                          "%Y %a, %m/%d")
            won = (line[2] == 'W')
            cubs.append((game_date, won))

    whitesox = []
    with open('whitesox.csv', 'r') as csvfile:
        reader = csv.reader(csvfile)
        for line in reader:
            if line[2][0:2] != 'vs':
                continue
            game_date = datetime.strptime(line[1], "%m-%d-%Y")

            won = line[-2] == 'W'
            whitesox.append((game_date, won))
    return cubs, whitesox


def get_football_games():
    games = []
    with open('nfl.csv', 'r') as csvfile:
        reader = csv.reader(csvfile)
        for line in reader:
            if line[-4] != "Chicago Bears":
                continue

            game_date = datetime.strptime(line[0], "%m/%d/%Y")
            home_score = int(line[-3])
            visitor_score = int(line[2])
            won = home_score > visitor_score
            games.append((game_date, won))

    return games

def get_hockey_games():
    games = []
    with open('hockey.csv', 'r') as csvfile:
        reader = csv.reader(csvfile)
        for line in reader:
            if len(line) == 0 or line[0] == "GP" or line[2] == "@":
                continue
            game_date = datetime.strptime(line[1], "%Y-%m-%d")
            won = line[4] == 'W'
            games.append((game_date, won))
    return games

def aggregate():
    def find_game(game_list, date):
        """
        Returns 1 if there was a game they won, 2 if they lost and 0
        if there was no game.
        """
        for item, won in game_list:
            if item == date:
                if won:
                    return 1
                return 2
        return 0
    start_time = datetime(2011, 1, 1)
    end_time = datetime(2013, 6, 1)
    current_day = start_time

    bulls = get_basketball_games()
    cubs, whitesox = get_baseball_games()
    bears = get_football_games()
    blackhawks = get_hockey_games()
    all_values = []
    while(current_day < end_time):
        values = (current_day.strftime("%Y-%m-%d"),
                  find_game(bulls, current_day),
                  find_game(bears, current_day),
                  find_game(cubs, current_day),
                  find_game(whitesox, current_day),
                  find_game(blackhawks, current_day))
        all_values.append(values)
        current_day += timedelta(days = 1)

    with open('sports_games.csv', 'w') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(("date", "bulls", "bears",
                         "cubs", "whitesox", "blackhawks"))
        writer.writerows(all_values)

aggregate()
