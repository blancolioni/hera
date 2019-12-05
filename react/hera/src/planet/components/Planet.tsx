import React from "react";
import * as THREE from "three";
import { noise3d, fractalNoise3d, ridgedNoise3d } from '../../_3d/Noise';
import { State } from '../model';
import { ClientDispatch } from '../../clients/model';
import Model3D from '../../_3d/Model3D';

interface Dispatch extends ClientDispatch {
  
}

interface Props {
    clientState : State,
    clientDispatch : Dispatch
  }

interface PlanetSceneState {
  planet : State | null
}

const standardVertexShader : string = `
  varying vec2 vUv;
  varying vec3 vNormal;

  void main() {
    vUv = uv;
    vNormal = normal;
    gl_Position = projectionMatrix *
                  modelViewMatrix *
                  vec4(position,1.0);
  }  
`

const gasGiantFragmentShader = `
varying vec2 vUv;
varying vec3 vNormal;
uniform sampler2D textureSampler;
uniform float unTime;
`

+ noise3d

+ fractalNoise3d

+ ridgedNoise3d

+ `
float computeDiffuse(vec3 normal) {
  return clamp( dot( normal, vec3(0.1, 0.0, 5.0) ), 0.0, 1.0 );
}

void main() {
  vec3 position = vNormal + vec3(unTime, 0.0, unTime);
  float n1 = noise(position, 6, 10.0, 0.8) * 0.01;
  float n2 = ridgedNoise(position, 5, 5.8, 0.75) * 0.015 - 0.01;
  // Get the three threshold samples

  float s = 0.6;
  float t1 = snoise(position * 2.0) - s;
  float t2 = snoise((position + 800.0) * 2.0) - s;
  float t3 = snoise((position + 1600.0) * 2.0) - s;
  
  // Intersect them and get rid of negatives
  
  float threshold = max(t1 * t2 * t3, 0.0);
  
  // Storms
  
  float n3 = snoise(position * 0.1) * threshold;
  
  float n = n1 + n2 + n3;
  float newTexCoord = vUv.y + n;
  vec4 texColor = texture2D(textureSampler, vec2(newTexCoord, 0.0));

  vec3 light = vec3(0.1, 0.0, 5.0);
  light = normalize(light);

  float dProd = max(0.0, dot(vNormal, light));

  gl_FragColor = texColor * vec4(dProd, dProd, dProd, 1.0);

}
`

const rockPlanetFragmentShader = `
  varying vec2 vUv;
  varying vec3 vNormal;
  uniform sampler2D textureSampler;
  uniform vec3 starLight;
  ` +
  noise3d +
  fractalNoise3d +
  `
  void main() {
    vec3 light = normalize(starLight);
    float n = noise(vNormal, 16, 1.5, 0.4) / 2.0 + 0.5;
    float dProd = max(0.0, dot(vNormal, light));
    vec4 texColor = texture2D(textureSampler, vec2(n, 0.0));
    gl_FragColor = texColor * vec4(dProd, dProd, dProd, 1.0);
  }
`

const PlanetTextureName = (planet : State) : string => {
  const { sectors } = planet;
  let textureName : string = '';
  
  if (sectors.length === 0) {
    textureName = 'gas_giant_jovian'
    // if (mass >= 100) {
    //   textureName = 'gas_giant_jovian'
    // } else if (mass >= 50) {
    //   textureName = 'gas_giant_saturnian'
    // } else if (mass >= 15) {
    //   textureName = 'gas_giant_blue'
    // } else {
    //   textureName = 'gas_giant_cyan'
    // }
  } else {
    textureName = 'temperate'
  }

  return textureName;
}

export function PlanetMesh(
  model : Model3D,
  planet : State,
  detail : number,
  lightFrom : THREE.Vector3,
) : THREE.Mesh
{

  let mesh : THREE.Mesh;
  const { sectors } = planet;

  if (sectors.length === 0) {
      const vertexShader = standardVertexShader;

      const fragmentShader = gasGiantFragmentShader;
      const textureName = PlanetTextureName(planet);
      const texture =  model.textureLoader.load(textureName + '.png');

      const geometry = new THREE.IcosahedronBufferGeometry(1, detail);
      const material = new THREE.ShaderMaterial({
        vertexShader,
        fragmentShader,
        uniforms: {
          textureSampler: { type: 't', value: texture },
          unTime: { type: 'f', value: 0},
          starLight: { value: lightFrom }
        },
      });

      mesh = new THREE.Mesh(geometry, material);
    } else {
      //let mats : THREE.Material[] = [];
      mesh = new THREE.Mesh();
      for(const sector of sectors) {
        let geometry = new THREE.Geometry();
        for (const vertex of sector.boundary) {
          const v = new THREE.Vector3(vertex.x, vertex.z, -vertex.y);
          geometry.vertices.push(v);
        }
        for (let i = 1; i < sector.boundary.length - 1; ++i) {
          const face = new THREE.Face3(0, i + 1, i,  new THREE.Vector3(sector.normal.x, sector.normal.z, -sector.normal.y));
          for (const vertex of sector.boundary) {
            const v = new THREE.Vector3(vertex.x, vertex.z, -vertex.y);
            face.vertexNormals.push(v);
          }
          geometry.faces.push(face);
        }

        const material = new THREE.MeshBasicMaterial({
            color: new THREE.Color(sector.color),
          });
        mesh.add(new THREE.Mesh(geometry, material));
      }
    }

    mesh!.name = planet.id;
    return mesh!;
}

class Planet extends React.Component<Props,PlanetSceneState> {

  mount: any;

  model  : Model3D | null;
  planetLo : THREE.Mesh | null;
  planetHi : THREE.Mesh | null;
  renderCount : number

  constructor(props : Props) {
    super (props);

    this.state = {
      planet: null,
    }

    this.renderCount = 0;
    this.planetLo = null;
    this.planetHi = null;
    this.model = null;
    this.beforeRender = this.beforeRender.bind(this);
  }

  componentDidMount() {
    this.model = new Model3D(this.mount, 0.1, 10, 0.1, 10);
    this.addCustomSceneObjects();
    this.model.startAnimationLoop(this.beforeRender);
  }

  componentWillUnmount() {
    this.model!.stopAnimationLoop();
  }

  addCustomSceneObjects = () => {
    this.model!.light.position.set(3, 0, 3);
    this.model!.scene.add(this.model!.light);
    // const light2 = new THREE.DirectionalLight();
    // light2.position.set(3, 0, -3);
    // this.model!.scene.add(light2);
  }

  beforeRender() {
    if (this.planetLo && !this.planetHi) {
      if (this.props.clientState && this.props.clientState.sectors.length === 0) {
        (this.planetLo!.material as THREE.ShaderMaterial).uniforms.unTime.value = this.renderCount;
      } 
    } else if (this.planetHi) {
        this.planetHi.rotateY(0.002);
    }
    
    this.renderCount += 0.0002;
  }

  updateScene = (planet : State) => {
    if (!this.planetLo) {
      this.planetLo = PlanetMesh(this.model!, planet, 6, new THREE.Vector3(-5, 0, 3));
      this.model!.scene.add(this.planetLo);
      this.props.clientDispatch.requestUpdate(2);
    } else if (planet.sectors.length > 0 && !this.planetHi) {
      this.planetHi = PlanetMesh(this.model!, planet, 6, new THREE.Vector3(-5, 0, 3));
      this.model!.scene.remove(this.planetLo);
      this.model!.scene.add(this.planetHi);
    }
  }

  render() {
    if (this.props.clientState && this.model) {
      console.log('render', this.props.clientState)
      this.updateScene(this.props.clientState);
    }
    return (
      <div className="harriet-model3d" ref={ref => (this.mount = ref)} />
    )
  }
}

export default Planet;
