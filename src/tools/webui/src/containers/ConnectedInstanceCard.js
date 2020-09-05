/**
 * @file
 *
 * @brief connect the InstanceCard component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {connect} from "react-redux";
import {bindActionCreators} from "redux";

import {deleteInstance, sendNotification, updateInstance} from "../actions";
import InstanceCard from "../components/InstanceCard.jsx";

const mapStateToProps = (state, { id }) => {
	return { instances: state.instances };
};

const mapDispatchToProps = dispatch => bindActionCreators ({ updateInstance, deleteInstance, sendNotification }, dispatch);

export default connect (mapStateToProps, mapDispatchToProps) (InstanceCard);
