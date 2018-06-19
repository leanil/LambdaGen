import { SyntaxTreeNodeModel } from "./SyntaxTreeNodeModel";

import * as _ from "lodash";
import * as React from "react";
import * as SRD from "storm-react-diagrams";

export interface ISyntaxTreeNodeProps<Model extends SyntaxTreeNodeModel> extends SRD.BaseWidgetProps {
	node: Model;
    diagramEngine: SRD.DiagramEngine;
}

export abstract class SyntaxTreeNodeWidget<Model extends SyntaxTreeNodeModel> extends SRD.BaseWidget<ISyntaxTreeNodeProps<Model>, any> {
	constructor(props: ISyntaxTreeNodeProps<Model>) {
		super("srd-default-node", props);
	}

	public generatePort(port: any) {
		return <SRD.DefaultPortLabel model={port} key={port.id} />;
	}

	public abstract properties(): JSX.Element;

	public render() {
		return (
			<div {...this.getProps()} style={{ background: this.props.node.color }}>
				<div className={this.bem("__title")}>
					<div className={this.bem("__name")}>{this.props.node.name}</div>
				</div>
				<div>
					{this.properties()}
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