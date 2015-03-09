#ifndef FINDVISITOR_HPP
#define FINDVISITOR_HPP

#include "visitor.hpp"
#include "confignode.hpp"

/**
 * @brief The FindVisitor class
 */
class FindVisitor : public Visitor
{
public:
	/**
	 * @brief FindVisitor
	 * @param searchResults
	 * @param term
	 */
	explicit FindVisitor(TreeViewModel* searchResults, const QString& term);

	/**
	 * @brief visit
	 * @param node
	 */
	void visit(ConfigNode& node);

	/**
	 * @brief visit
	 * @param model
	 */
	void visit(TreeViewModel* model);

private:
	TreeViewModel*	m_searchResults;
	QString			m_term;
};

#endif // FINDVISITOR_HPP
