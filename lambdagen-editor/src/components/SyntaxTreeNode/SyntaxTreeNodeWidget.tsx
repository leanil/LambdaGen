import { SyntaxTreeNodeModel } from "./SyntaxTreeNodeModel";

import * as _ from "lodash";
import * as React from "react";
import * as SRD from "storm-react-diagrams";

export interface ISyntaxTreeNodeProps extends SRD.BaseWidgetProps {
	node: SyntaxTreeNodeModel;
    diagramEngine: SRD.DiagramEngine;
}

export class SyntaxTreeNodeWidget extends SRD.BaseWidget<ISyntaxTreeNodeProps, any> {
	constructor(props: ISyntaxTreeNodeProps) {
		super("srd-default-node", props);
		this.state = {value: ''};

		this.handleChange = this.handleChange.bind(this);
	}

	public generatePort(port: any) {
		return <SRD.DefaultPortLabel model={port} key={port.id} />;
	}

	public handleChange(event: any) {
		this.setState({value: event.target.value});
	}

	public render() {
		return (
			<div {...this.getProps()} style={{ background: this.props.node.color }}>
				<div className={this.bem("__title")}>
					<div className={this.bem("__name")}>{this.props.node.name}</div>
					<input type="number" value={this.state.value} onChange={this.handleChange} />
				</div>
				<div className={this.bem("__ports")}>
					<div className={this.bem("__in")}>
						{_.map(this.props.node.getInPorts(), this.generatePort.bind(this))}
					</div>
					<div className={this.bem("__out")}>
						{_.map(this.props.node.getOutPorts(), this.generatePort.bind(this))}
					</div>
				</div>
			</div>
		);
	}
}