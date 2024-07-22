package uk.gov.hmrc.agentinvitationsfrontend.support

object Css {
  val ERROR_SUMMARY_TITLE = ".govuk-error-summary__title"
  val ERROR_SUMMARY_LINK = ".govuk-list.govuk-error-summary__list li a"
  val errorSummaryLinkWithHref = (href: String) => s".govuk-list.govuk-error-summary__list li a[href=$href]"
  val H1 = "main h1"
  val PRE_H1 = "main .govuk-caption-l"
  val paragraphs = "main p"
  def errorSummaryForField(id: String): String =
    s".govuk-error-summary__body li a[href=#$id]"
  def errorForField(id: String): String = s"span#$id-error"
  def labelFor(id: String): String = s"label[for=$id]"
  val SUBMIT_BUTTON = "main form button"
  val currentLanguage = "ul.hmrc-language-select__list li.hmrc-language-select__list-item span[aria-current=true]";
  val alternateLanguage = ".hmrc-language-select__list .hmrc-language-select__list-item a.govuk-link";
}
