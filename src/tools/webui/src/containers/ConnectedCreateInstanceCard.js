/**
 * @file
 *
 * @brief connect the CreateInstanceCard component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {connect} from "react-redux";
import {bindActionCreators} from "redux";

import {createInstance, sendNotification, unaddInstance} from "../actions";
import CreateInstanceCard from "../components/CreateInstanceCard.jsx";

const mapStateToProps = state => {
	return { instances: state.instances };
};

const mapDispatchToProps = dispatch => bindActionCreators ({ createInstance, unaddInstance, sendNotification }, dispatch);

export default connect (mapStateToProps, mapDispatchToProps) (CreateInstanceCard);
