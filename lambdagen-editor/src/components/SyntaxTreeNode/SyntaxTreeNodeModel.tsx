import * as _ from "lodash";
import * as SRD from "storm-react-diagrams";

export class SyntaxTreeNodeModel extends SRD.NodeModel {

	public name: string;
	public color: string;
	public ports: { [s: string]: SRD.DefaultPortModel };

	constructor(nodeType: string, name: string, color: string, ports: string[]) {
		super(nodeType);
		this.name = name;
		this.color = color;
		for(const portName of ports) {
			this.addPort(new SRD.DefaultPortModel(true,portName,portName));
		}
		this.addPort(new SRD.DefaultPortModel(false,"result","result"));
	}

	public getInPorts(): SRD.DefaultPortModel[] {
		return _.filter(this.ports, portModel => {
			return portModel.in;
		});
	}

	public getOutPorts(): SRD.DefaultPortModel[] {
		return _.filter(this.ports, portModel => {
			return !portModel.in;
		});
	}
}
