import * as _ from "lodash";
import * as SRD from "storm-react-diagrams";

export class SyntaxTreeNodeModel extends SRD.NodeModel {

	public name: string = "SclOp"
	public color: string = "rgb(0,192,255)";
	public ports: { [s: string]: SRD.DefaultPortModel };

	constructor() {
        super("syntaxTree");
        this.addPort(new SRD.DefaultPortModel(true, "left", "left"));
        this.addPort(new SRD.DefaultPortModel(true, "right", "right"));
        this.addPort(new SRD.DefaultPortModel(false, "result", "result"));
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
