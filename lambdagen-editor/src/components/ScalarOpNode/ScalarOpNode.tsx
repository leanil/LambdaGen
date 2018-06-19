import { SyntaxTreeNodeFactory } from "../SyntaxTreeNode/SyntaxTreeNodeFactory";
import { SyntaxTreeNodeModel } from "../SyntaxTreeNode/SyntaxTreeNodeModel";
import { ScalarOpNodeWidget } from "./ScalarOpNodeWidget"

import * as _ from "lodash";

export class ScalarOpNodeModel extends SyntaxTreeNodeModel {

    public value: string;

    constructor() {
        super("scalarOpNode","ScalarOp","rgb(0,192,255)",["left","right"]);
    }

    public serialize() {
		return _.merge(super.serialize(), {
			expr: {
				tag: "ScalarOp",
				getOp: this.value
			}
		});
	}
}

export class ScalarOpNodeFactory extends SyntaxTreeNodeFactory<ScalarOpNodeModel,ScalarOpNodeWidget> {
	constructor() {
		// TODO: why does this help? (ReferenceError: ScalarNodeModel is not defined)
		const SNM = ScalarOpNodeModel;
		const SNW = ScalarOpNodeWidget;
		super(SNM,SNW,"scalarOpNode");
	}
}