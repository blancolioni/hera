import * as t from './actionTypes';
import * as clients from '../clients/actionTypes';
import { State, planetInitialState } from './model';
import { PlanetActionTypes } from './actions';
import { clientInitialState } from '../clients/model';
import { ClientActionTypes } from '../clients/actions';

export default (state = planetInitialState(clientInitialState), action: PlanetActionTypes | ClientActionTypes): State => {
  switch (action.type) {

    case clients.UPDATE_CLIENT:
        return {
            ...state,
            ...action.newState,
        }
    default:
        return state;
  }
};
