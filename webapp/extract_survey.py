import sys
import json
import pandas as pd

def main(fn, output):
    responses = []
    with open(fn) as f:
        for line in f:
            obj = json.loads(line)
            userid = obj['inputs']['userid']

            if obj['event'] != 'response':
                continue

            responses.append({
                'userid': userid,
                'response': obj['extra_data']['resp'],
                'reversed_scale': obj['params']['reversed_scale'],
                })

    r = pd.DataFrame(responses)
    r.to_csv(output)

if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
