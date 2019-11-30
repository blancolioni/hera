import React from "react";
import { State, Sector } from '../model';
import { ClientDispatch } from '../../clients/model';

interface Dispatch extends ClientDispatch {
}

interface Props {
    clientState : State,
    clientDispatch : Dispatch
}

class Component extends React.Component<Props,State> {

  constructor(props : Props) {
    super (props);

  }


  render() {

    console.log(this.props);
    if (this.props.clientState.width > 0) {
      let { sectors, width, height } = this.props.clientState;
      let index = 0;
      let rows : Sector[][] = [];
      for (let y=0; y < height; ++y) {
        let cols : Sector[] = [];
        for (let x = 0; x < width; ++x) {
          cols.push(sectors[index++]);
        }
        rows.push(cols);
      }

      return (
        <table>
          { 
            rows.map(row => {
              return (
                <tr>
                  {row.map((col) => {
                    return (
                      <td><img src={'images/planets/terrain/' + col.terrain + '.png'}></img></td>
                    )
                  })
                  }
                </tr>
              );
            })
          }
        </table>
      )

    } else {
      return (
        <div>
          Loading ...
        </div>
      );
    }
  }
}

export default Component;
