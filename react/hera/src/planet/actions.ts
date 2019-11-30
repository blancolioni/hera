import * as t from './actionTypes';
import { ClientDispatch } from '../clients/model';
import { ClientAction } from '../clients/actions';

export interface InspectAction extends ClientAction {
    type: typeof t.INSPECT
    x: number,
	y: number,
  }
  
export type PlanetActionTypes = InspectAction

export function inspect(clientId : number, x : number, y : number) : PlanetActionTypes {
    return {
        type: t.INSPECT,
        clientId,
        x, 
		y
        }
}
