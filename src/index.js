import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    clientSecret: process.env.ELM_APP_STRAVA_CLIENT_SECRET,
    clientId: process.env.ELM_APP_STRAVA_CLIENT_ID,
    origin: process.env.ELM_APP_ORIGIN,
    dev: process.env.NODE_ENV === 'development',
  },
});

registerServiceWorker();
