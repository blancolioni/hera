import * as t from './actionTypes';
import { ClientDispatch } from '../clients/model';
import { ClientAction } from '../clients/actions';

export interface ZoomAction extends ClientAction {
    type: typeof t.ZOOM
    name: string
  }
  
export type GalaxyActionTypes = ZoomAction

export function zoom(clientId : number, name: string) : GalaxyActionTypes {
    return {
        type: t.ZOOM,
        clientId,
        name
        }
}
