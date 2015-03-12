#ifndef FINDVISITOR_HPP
#define FINDVISITOR_HPP

#include "visitor.hpp"
#include "confignode.hpp"

/**
 * @brief The FindVisitor class. It performs the search for a searchterm and includes all ConfigNode s that contain the search
 * term in their name, value or metadata.
 */
class FindVisitor : public Visitor
{
public:
	/**
	 * @brief FindVisitor The constructor.
	 * @param searchResults An empty TreeViewModel that will contain the ConfigNode s that match the searchterm after the search
	 * is completed.
	 * @param term The search term to look for.
	 */
	explicit FindVisitor(TreeViewModel* searchResults, const QString& term);

	/**
	 * @copydoc Visitor::visit()
	 */
	void visit(ConfigNode& node);

	/**
	 * @copydoc Visitor::visit()
	 */
	void visit(TreeViewModel* model);

private:
	TreeViewModel*	m_searchResults;
	QString			m_term;
};

#endif // FINDVISITOR_HPP
