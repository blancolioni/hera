import * as t from './actionTypes';
import * as clients from '../clients/actionTypes';
import { State, galaxyInitialState } from './model';
import { GalaxyActionTypes } from './actions';
import { clientInitialState } from '../clients/model';
import { ClientActionTypes } from '../clients/actions';

export default (state = galaxyInitialState(clientInitialState), action: GalaxyActionTypes | ClientActionTypes): State => {
  switch (action.type) {

    case clients.UPDATE_CLIENT:
        return {
            ...state,
            ...action.newState,
        }
    case t.ZOOM:
    case clients.ZOOM_OBJECT:
      console.log("zoom", action)
        return {
          ...state,
          zoom: action.name
        }
    default:
        return state;
  }
};
