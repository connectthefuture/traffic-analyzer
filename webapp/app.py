import csv
import json
from datetime import datetime

from flask import Flask, render_template, request
from rapper import predict_data

app = Flask(__name__)

REGIONS = {}

# All of this should be elsewher
def load_region_list():
    with open('../dat/single_region_data.csv') as csvfile:
        reader = csv.reader(csvfile)
        next(reader, None)
        for row in reader:
            # store south west and north east corners
            REGIONS[int(row[1])] = [(float(row[4]), float(row[2])),
                                    (float(row[5]), float(row[3]))]


def get_values(time, cubs, bulls, bears, whitesox, blackhawks):
    result = []
    speeds = predict_data(time, cubs, bulls, bears, whitesox, blackhawks)
    print(speeds)
    print("Woo!")
    for speed, region_id in speeds:
        result.append((REGIONS[region_id], speed))
    return result
# END HACK

@app.route('/request_data/')
def request_data():
    time = datetime.strptime(request.args.get('time'),
                             "%Y-%m-%dT%H:%M")
    cubs = request.args.get('cubs') == 'true'
    bulls = request.args.get('bulls') == 'true'
    bears = request.args.get('bears') == 'true'
    whitesox = request.args.get('whitesox') == 'true'
    blackhawks = request.args.get('blackhawks') == 'true'
    return json.dumps(get_values(time, cubs, bulls, bears, whitesox, blackhawks))



@app.route('/')
def index():
    return render_template('index.html')

if __name__ == '__main__':
    load_region_list()
    app.run(debug=True)
