"""
A r wrapper.
"""

#pylint: disable=all
from datetime import datetime

import rpy2.robjects as robjects

SET_UP = False

def rapper_setup():
    global SET_UP
    if SET_UP:
        return
    # Load up the library
    robjects.r("library('locfit')")
    # Load up my models
    for i in range(1, 29):
        print "Loading {0}".format(i)
        robjects.r("fit{0} <- readRDS('../models/{0}-nonparametric.RData')".format(i))

    SET_UP = True

def predict_data(predict_time, cubs, bulls, bears, whitesox, blackhawks):
    """
    Takes in a prediction time and generates predictions.
    """

    rapper_setup()

    if predict_time.weekday() >= 5:
        weekend = 1
    else:
        weekend = 0


    # Construct a day label
    days_since_start = (predict_time - datetime(2011, 3, 12)).days + 1

    robjects.r("day_label = {0}".format(days_since_start))
    robjects.r("year = {0}".format(predict_time.year))
    robjects.r("month = {0}".format(predict_time.month))
    robjects.r("day = {0}".format(predict_time.day))
    robjects.r("hour = {0}".format(predict_time.hour))
    robjects.r("minute = {0}".format(predict_time.minute))
    robjects.r("weekday = {0}".format(predict_time.weekday()))
    robjects.r("weekend = {0}".format(weekend))
    robjects.r("any_game = {0}".format(int(bears or whitesox or bulls or
                                           cubs or blackhawks)))
    robjects.r("bears = {0}".format(int(bears)))
    robjects.r("whitesox = {0}".format(int(whitesox)))
    robjects.r("bulls = {0}".format(int(bulls)))
    robjects.r("cubs = {0}".format(int(cubs)))
    robjects.r("blackhawks = {0}".format(int(blackhawks)))

    robjects.r("df = data.frame(year, month, day, hour, minute, weekday, weekend, bears, whitesox, bulls, cubs, blackhawks)")
    results = [(robjects.r("predict(fit{0},df)".format(x))[0], x)
                for x in range(1, 29)]

    return results
