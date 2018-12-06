
package uk.gov.hmrc.agentinvitationsfrontend.views.html.agents

import play.twirl.api._
import play.twirl.api.TemplateMagic._


     object known_fact_Scope0 {
import models._
import controllers._
import play.api.i18n._
import views.html._
import play.api.templates.PlayMagic._
import play.api.mvc._
import play.api.data._

     object known_fact_Scope1 {
import play.api.Configuration
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes
import uk.gov.hmrc.play.views.html.helpers.{errorSummary, form, dateFieldsFreeInlineLegend}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.play.views.html.helpers.input
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.back_link
import uk.gov.hmrc.agentinvitationsfrontend.views.html._
import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentAuthorisationRequest

class known_fact extends BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with play.twirl.api.Template6[Form[CurrentAuthorisationRequest],String,Request[_$1] forSome { 
   type _$1 >: _root_.scala.Nothing <: _root_.scala.Any
},Messages,Configuration,ExternalUrls,play.twirl.api.HtmlFormat.Appendable] {

  /**/
  def apply/*26.2*/(knownFactForm: Form[CurrentAuthorisationRequest], serviceMessageKey: String)(implicit request: Request[_], messages: Messages, configuration: Configuration, externalUrls: ExternalUrls):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {

def /*28.2*/noJsDateFieldsError/*28.21*/ = {{
 if(knownFactForm("knownFact.day").hasErrors || knownFactForm("knownFact.month").hasErrors || knownFactForm("knownFact.year").hasErrors)
  "nojs-date-fields-error"
}};def /*33.2*/flushHeading/*33.14*/ = {{if(knownFactForm("knownFact").hasErrors || knownFactForm("knownFact.day").hasErrors || knownFactForm("knownFact.month").hasErrors || knownFactForm("knownFact.year").hasErrors) " flush--top" else ""}};def /*35.2*/pageHeading/*35.13*/ = {{"""<h1 class="heading-xlarge margin-bottom-30"""+ flushHeading  + """">""" + Messages(s"known-fact.${knownFactForm("service").value.getOrElse("")}.heading") + """</h1>""" + """<p>""" + Messages("known-fact.p1") + """</p>"""}};
Seq[Any](format.raw/*26.187*/("""

"""),format.raw/*31.2*/("""

"""),format.raw/*33.217*/("""

"""),format.raw/*35.242*/("""


"""),_display_(/*38.2*/uk/*38.4*/.gov.hmrc.agentinvitationsfrontend.views.html.main_template(title = error_prefix(knownFactForm) + Messages("generic.title", Messages(s"known-fact.${knownFactForm("service").value.getOrElse("")}.heading"), Messages("title.suffix.agents")), bodyClasses = None, isAgent = true, mainDataAttributes = Some(gaPageEvent(Messages(s"known-fact.${knownFactForm("service").value.getOrElse("")}.heading"), serviceMessageKey)))/*38.418*/ {_display_(Seq[Any](format.raw/*38.420*/("""

 """),_display_(/*40.3*/back_link(s"${routes.AgentsInvitationController.checkDetails()}")),format.raw/*40.68*/("""

 """),_display_(/*42.3*/if(knownFactForm.hasErrors)/*42.30*/ {_display_(Seq[Any](format.raw/*42.32*/("""
  """),_display_(/*43.4*/errorSummary(Messages("error.summary.heading"), knownFactForm, Seq.empty, Some("known-fact"))),format.raw/*43.97*/("""
 """)))}),format.raw/*44.3*/("""


 """),_display_(/*47.3*/form(
  action = routes.AgentsInvitationController.submitKnownFact(),
  'class -> "form js-form")/*49.28*/ {_display_(Seq[Any](format.raw/*49.30*/("""

  """),_display_(/*51.4*/if(knownFactForm("service").value.getOrElse("") == "HMRC-MTD-IT")/*51.69*/ {_display_(Seq[Any](format.raw/*51.71*/("""
   """),format.raw/*52.4*/("""<div class="form-group">
   """),_display_(/*53.5*/input(
    knownFactForm("knownFact"),
    '_label -> Html(pageHeading),
    '_inputClass -> "form-control",
    '_inputHint -> Messages(s"known-fact.${knownFactForm("service").value.getOrElse("")}.helper"),
    '_labelClass -> "soft--ends"
   )),format.raw/*59.5*/("""
   """),format.raw/*60.4*/("""</div>
  """)))}/*61.4*/else/*61.9*/{_display_(Seq[Any](format.raw/*61.10*/("""
   """),format.raw/*62.4*/("""<div class=""""),_display_(/*62.17*/noJsDateFieldsError),format.raw/*62.36*/("""">
   """),_display_(/*63.5*/dateFieldsFreeInlineLegend(knownFactForm, "knownFact",
    '_legend -> Html(pageHeading),
    '_inputHint -> Messages(s"known-fact.${knownFactForm("service").value.getOrElse("")}.helper")
   )),format.raw/*66.5*/("""
   """),format.raw/*67.4*/("""</div>
  """)))}),format.raw/*68.4*/("""

  """),format.raw/*70.3*/("""<input type="hidden" name="clientType" id="clientType" value=""""),_display_(/*70.66*/knownFactForm("clientType")/*70.93*/.value),format.raw/*70.99*/("""">
  <input type="hidden" name="service" id="service" value=""""),_display_(/*71.60*/knownFactForm("service")/*71.84*/.value.getOrElse("")),format.raw/*71.104*/("""">
  <input type="hidden" name="clientIdentifier" id="clientIdentifier" value=""""),_display_(/*72.78*/knownFactForm("clientIdentifier")/*72.111*/.value.getOrElse("")),format.raw/*72.131*/("""">
  <input type="hidden" name="clientIdentifierType" id="clientIdentifierType" value=""""),_display_(/*73.86*/knownFactForm("clientIdentifierType")/*73.123*/.value.getOrElse("")),format.raw/*73.143*/("""">

  <div class="form-group">
   <button class="button" type="submit" id="continue">"""),_display_(/*76.56*/Messages("continue.button")),format.raw/*76.83*/("""</button>
  </div>

 """)))}),format.raw/*79.3*/("""
""")))}),format.raw/*80.2*/("""

"""))
      }
    }
  }

  def render(knownFactForm:Form[CurrentAuthorisationRequest],serviceMessageKey:String,request:Request[_$1] forSome { 
   type _$1 >: _root_.scala.Nothing <: _root_.scala.Any
},messages:Messages,configuration:Configuration,externalUrls:ExternalUrls): play.twirl.api.HtmlFormat.Appendable = apply(knownFactForm,serviceMessageKey)(request,messages,configuration,externalUrls)

  def f:((Form[CurrentAuthorisationRequest],String) => (Request[_$1] forSome { 
   type _$1 >: _root_.scala.Nothing <: _root_.scala.Any
},Messages,Configuration,ExternalUrls) => play.twirl.api.HtmlFormat.Appendable) = (knownFactForm,serviceMessageKey) => (request,messages,configuration,externalUrls) => apply(knownFactForm,serviceMessageKey)(request,messages,configuration,externalUrls)

  def ref: this.type = this

}


}
}

/**/
object known_fact extends known_fact_Scope0.known_fact_Scope1.known_fact
              /*
                  -- GENERATED --
                  DATE: Thu Dec 06 08:37:32 GMT 2018
                  SOURCE: /home/marianne/hmrc/agent-invitations-frontend/app/uk/gov/hmrc/agentinvitationsfrontend/views/agents/known_fact.scala.html
                  HASH: e47f1ad5d4a8f6de2fab5775e2022ee270fff927
                  MATRIX: 1273->1121|1537->1309|1565->1328|1750->1502|1771->1514|1988->1720|2008->1731|2269->1306|2298->1499|2329->1717|2360->1960|2390->1964|2400->1966|2824->2380|2865->2382|2895->2386|2981->2451|3011->2455|3047->2482|3087->2484|3117->2488|3231->2581|3264->2584|3295->2589|3401->2686|3441->2688|3472->2693|3546->2758|3586->2760|3617->2764|3672->2793|3937->3038|3968->3042|3996->3052|4008->3057|4047->3058|4078->3062|4118->3075|4158->3094|4191->3101|4403->3293|4434->3297|4474->3307|4505->3311|4595->3374|4631->3401|4658->3407|4747->3469|4780->3493|4822->3513|4929->3593|4972->3626|5014->3646|5129->3734|5176->3771|5218->3791|5331->3877|5379->3904|5431->3926|5463->3928
                  LINES: 32->26|36->28|36->28|39->33|39->33|39->35|39->35|40->26|42->31|44->33|46->35|49->38|49->38|49->38|49->38|51->40|51->40|53->42|53->42|53->42|54->43|54->43|55->44|58->47|60->49|60->49|62->51|62->51|62->51|63->52|64->53|70->59|71->60|72->61|72->61|72->61|73->62|73->62|73->62|74->63|77->66|78->67|79->68|81->70|81->70|81->70|81->70|82->71|82->71|82->71|83->72|83->72|83->72|84->73|84->73|84->73|87->76|87->76|90->79|91->80
                  -- GENERATED --
              */
          