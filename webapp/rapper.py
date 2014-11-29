"""
A r wrapper.
"""

#pylint: disable=all
from datetime import datetime

import rpy2.robjects as robjects

# Load up the library
robjects.r("library('locfit')")
# Load up my models
#robjects.r("load('../region13.RData')")


def predict_data(predict_time, cubs, bulls, bears, whitesox, blackhawks):
    """
    Takes in a prediction time and generates predictions.
    """

    if predict_time.weekday() >= 5:
        weekend = 1
    else:
        weekend = 0

    """
    days_since_start = (predict_time - datetime(2011, 3, 12)).days + 1
    robjects.r("day_label = {0}".format(days_since_start))
    robjects.r("year = {0}".format(predict_time.year))
    robjects.r("month = {0}".format(predict_time.month))
    robjects.r("day = {0}".format(predict_time.day))
    robjects.r("hour = {0}".format(predict_time.hour))
    robjects.r("minute = {0}".format(predict_time.minute))
    robjects.r("weekday = {0}".format(predict_time.weekday()))
    robjects.r("weekend = {0}".format(weekend))
    robjects.r("df = data.frame(year, month, day, hour, minute, weekday, weekend)")
    result = robjects.r("predict(fit,df)")
    """
    result = [1]
    return [(result[0], x) for x in range(1, 29)]
