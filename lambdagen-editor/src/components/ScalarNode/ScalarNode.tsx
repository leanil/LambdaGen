import { SyntaxTreeNodeFactory } from "../SyntaxTreeNode/SyntaxTreeNodeFactory";
import { SyntaxTreeNodeModel } from "../SyntaxTreeNode/SyntaxTreeNodeModel";
import { ScalarNodeWidget } from "./ScalarNodeWidget"

import * as _ from "lodash";

export class ScalarNodeModel extends SyntaxTreeNodeModel {

    public value: number;

    constructor() {
        super("scalarNode","Scalar","rgb(192,255,0)",[]);
    }

    public serialize() {
		return _.merge(super.serialize(), {
			expr: {
				tag: "Scalar",
				getSclVal: this.value
			}
		});
	}
}

export class ScalarNodeFactory extends SyntaxTreeNodeFactory<ScalarNodeModel,ScalarNodeWidget> {
	constructor() {
		// TODO: why does this help? (ReferenceError: ScalarNodeModel is not defined)
		const SNM = ScalarNodeModel;
		const SNW = ScalarNodeWidget;
		super(SNM,SNW,"scalarNode");
	}
}