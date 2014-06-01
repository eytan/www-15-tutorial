
import json
from uuid import uuid4
from itertools import islice, cycle

from tornado.ioloop import IOLoop
from tornado.web import Application, RequestHandler, StaticFileHandler
from tornado.httpserver import HTTPServer

from planout.experiment import SimpleExperiment
from planout.ops.random import *

class CueExperiment(SimpleExperiment):
    """
    Randomly reorder the stories and choose from available cues.
    """
    def setup(self):
        self.set_log_file('logs/experiment.log')

    def assign(self, params, userid, story_keys):
        params.story_keys = Sample(choices=story_keys, unit=userid)
        balanced_sources = list(islice(cycle([
            'msnbc',
            'cnn',
            'foxnews',
        ]), len(story_keys)))
        params.sources = Sample(choices=balanced_sources, unit=userid)

class Likert(SimpleExperiment):
    """
    A randomization of the survey scale for robustness of the politics measure.
    """

    def setup(self):
        self.set_log_file('logs/survey.log')

    def assign(self, params, userid):
        params.reversed_scale = UniformChoice(choices=[0,1], unit=userid)


class BaseRequestHandler(RequestHandler):
    def get_current_user(self):
        userid = self.get_secure_cookie('userid')
        if not userid:
            userid = str(uuid4())
            self.set_secure_cookie('userid', userid)
        return userid

class PoliticalSurvey(BaseRequestHandler):
    
    scale = list(enumerate([
            'very conservative',
            'conservative',
            'moderate',
            'liberal',
            'very liberal',
        ]))

    def get(self):
        if self.get_secure_cookie('survey_taken'):
            pass #self.redirect('/thanks')

        exp = Likert(userid=self.current_user)

        if exp.get('reversed_scale'):
            scale = list(reversed(self.scale))
        else:
            scale = self.scale

        self.render(
            'templates/survey.html',
            scale=scale
        )

    def post(self):

        resp = self.get_argument('politics', None)
        if resp is None:
            self.redirect('/survey')
        else:
            resp = int(resp)

        exp = Likert(userid=self.current_user)

        exp.log_event('response', {'resp': resp})

        self.set_secure_cookie('survey_taken', '1')
        self.redirect('/thanks')

class Thanks(BaseRequestHandler):
    def get(self):
        self.render('templates/thanks.html')

class SummariseContent(BaseRequestHandler):
    with open('stories.json') as f:
        stories = {int(k): v for k, v in json.load(f).iteritems()}
    story_keys = sorted(stories.keys())
 
    def get(self):
        exp = CueExperiment(
            userid=self.current_user,
            story_keys=self.story_keys,
        )

        stories = [(k, self.stories[k]) for k in exp.get('story_keys')]
        self.render(
            'templates/story.html', 
            k=3,
            sources=exp.get('sources'),
            stories=stories,
        )

    def post(self):
        exp = CueExperiment(
            userid=self.current_user,
            story_keys=self.story_keys
        )
        exp.log_event('summary', self.request.arguments)
        self.redirect('/survey')


if __name__ == '__main__':
    app = Application(
        [
            (r'/', SummariseContent),
            (r'/survey', PoliticalSurvey),
            (r'/thanks', Thanks),
            (r'/static/(.*)', StaticFileHandler, {"path": "static"})
        ],
        debug=False,
        static_path='static',
        cookie_secret='0.2752290303981113',
    )
    http_server = HTTPServer(app)
    http_server.listen(9000, address='127.0.0.1')
    IOLoop.instance().start()
