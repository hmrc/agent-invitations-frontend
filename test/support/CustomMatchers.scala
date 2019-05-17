/*
 * Copyright 2019 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package support

import org.jsoup.Jsoup
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{Assertions, Matchers}
import play.api.i18n.Messages
import play.twirl.api.{Html, HtmlFormat}

object CustomMatchers extends Matchers {
  type MessagesProvider = Messages

  protected def htmlEscapedMessage(key: String, params: String*)(implicit messagesProvider: MessagesProvider): String =
    HtmlFormat.escape(Messages(key, params: _*)).toString

  protected def checkMessageIsDefined(messageKey: String)(implicit messagesProvider: MessagesProvider) =
    Assertions.withClue(s"Message key ($messageKey) should be defined: ") {
      Messages.isDefinedAt(messageKey) shouldBe true
    }

  def havePageTitle(expectedTitleMessageKey: String, expectedMessageParamKeys: String*)(
    implicit messagesProvider: MessagesProvider): Matcher[Html] =
    new Matcher[Html] {
      override def apply(html: Html): MatchResult = {
        checkMessageIsDefined(expectedTitleMessageKey)
        expectedMessageParamKeys.foreach(checkMessageIsDefined)

        val doc = Jsoup.parse(html.toString)
        val foundTitles = doc.getElementsByTag("title")

        Assertions.withClue(s"There is more than 1 <title> tag in the page") {
          foundTitles.size should not be >(1)
        }
        Assertions.withClue(s"There is no <title> tag in the page") {
          foundTitles.size shouldBe 1
        }

        val foundTitleText = foundTitles.first().text()
        val expectedTitleText =
          htmlEscapedMessage(expectedTitleMessageKey, expectedMessageParamKeys.map(htmlEscapedMessage(_)): _*)
        val hasCorrectTitle = foundTitleText.equals(expectedTitleText)

        MatchResult(
          hasCorrectTitle,
          s"""Page's title of "$foundTitleText" does not match the expected title of "$expectedTitleText"""",
          s"""Page's title matches the unexpected title "$expectedTitleText""""
        )
      }
    }

  def containMessages(expectedMessageKeys: String*)(expectHtmlEscaped: Boolean = true)(
    implicit messagesProvider: MessagesProvider): Matcher[Html] =
    new Matcher[Html] {
      override def apply(html: Html): MatchResult = {
        expectedMessageKeys.foreach(checkMessageIsDefined)

        val htmlText = html.toString
        val (msgsPresent, msgsMissing) = expectedMessageKeys.partition { messageKey =>
          val expectedMessage =
            if (expectHtmlEscaped)
              htmlEscapedMessage(messageKey)
            else
              messagesProvider(messageKey)

          htmlText.contains(expectedMessage)
        }
        MatchResult(
          msgsMissing.isEmpty,
          s"Content is missing in the html for message keys: ${msgsMissing.mkString(", ")}",
          s"Content is present in the html for message keys: ${msgsPresent.mkString(", ")}"
        )
      }
    }

  def containMessageWithParams(expectedMessageKey: String, expectedMessageParameters: String*)(
    expectHtmlEscaped: Boolean = true)(implicit messagesProvider: MessagesProvider): Matcher[Html] =
    new Matcher[Html] {
      override def apply(html: Html): MatchResult = {
        checkMessageIsDefined(expectedMessageKey)

        val expectedMessage =
          if (expectHtmlEscaped)
            htmlEscapedMessage(expectedMessageKey, expectedMessageParameters: _*)
          else
            messagesProvider(expectedMessageKey, expectedMessageParameters: _*)

        val msgIsPresent = html.toString.contains(expectedMessage)

        MatchResult(
          msgIsPresent,
          s"Content is missing in the html for message key: $expectedMessageKey with params: ${expectedMessageParameters
            .mkString(", ")}. Expected message was '$expectedMessage'",
          s"Content is present in the html for message keys: $expectedMessageKey with params: ${expectedMessageParameters
            .mkString(", ")}. Message was '$expectedMessage'"
        )
      }
    }

  def containSubstrings(expectedSubstrings: String*): Matcher[Html] =
    new Matcher[Html] {
      override def apply(html: Html): MatchResult = {
        val htmlText = html.toString
        val (strsPresent, strsMissing) = expectedSubstrings.partition { expectedSubstring =>
          expectedSubstring.trim should not be ""
          htmlText.contains(expectedSubstring)
        }

        MatchResult(
          strsMissing.isEmpty,
          s"Expected substrings are missing in the response: ${strsMissing.mkString("\"", "\", \"", "\"")}",
          s"Expected substrings are present in the response : ${strsPresent.mkString("\"", "\", \"", "\"")}"
        )
      }
    }
  def containElement(id: String, tag: String, attrs: Map[String, String]): Matcher[Html] =
    new Matcher[Html] {
      override def apply(html: Html): MatchResult = {
        val doc = Jsoup.parse(html.toString)
        val foundElement = doc.getElementById(id)
        val isAsExpected = Option(foundElement) match {
          case None => false
          case Some(elFound) => {
            val isExpectedTag = elFound.tagName() == tag
            val hasExpectedAttrs = attrs.forall {
              case (expectedAttr, expectedValue) =>
                elFound.attr(expectedAttr) == expectedValue
            }
            isExpectedTag && hasExpectedAttrs
          }
        }
        MatchResult(
          isAsExpected,
          s"""Response does not contain a "$tag" element with id of "$id" with matching attributes $attrs""",
          s"""Response contains a "$tag" element with id of "$id" with matching attributes $attrs"""
        )
      }
    }
  def containElement(tag: String, expectedMessageKey: String)(
    implicit messagesProvider: MessagesProvider): Matcher[Html] =
    new Matcher[Html] {
      override def apply(html: Html): MatchResult = {
        checkMessageIsDefined(expectedMessageKey)
        val expectedContent = htmlEscapedMessage(expectedMessageKey)
        val doc = Jsoup.parse(html.toString)
        val foundElements = doc.getElementsByTag(tag)
        import collection.JavaConverters._
        val hasTagWithExpectedContent = foundElements.eachText().asScala.contains(expectedContent)
        MatchResult(
          hasTagWithExpectedContent,
          s"""Response does not contain a "$tag" element with content "$expectedContent"""",
          s"""Response contains a "$tag" element with content "$expectedContent""""
        )
      }
    }
  def containSubmitButton(
    expectedMessageKey: String,
    expectedElementId: String,
    expectedTagName: String = "button",
    expectedType: String = "submit")(implicit messagesProvider: MessagesProvider): Matcher[Html] =
    new Matcher[Html] {
      override def apply(html: Html): MatchResult = {
        val doc = Jsoup.parse(html.toString)
        checkMessageIsDefined(expectedMessageKey)
        val foundElement = doc.getElementById(expectedElementId)
        val isAsExpected = Option(foundElement) match {
          case None => false
          case Some(element) => {
            val isExpectedTag = element.tagName() == expectedTagName
            val isExpectedType = element.attr("type") == expectedType
            val hasExpectedMsg = element.text() == messagesProvider.messages(expectedMessageKey)
            isExpectedTag && isExpectedType && hasExpectedMsg
          }
        }
        MatchResult(
          isAsExpected,
          s"""Response does not contain a submit button with id "$expectedElementId" and type "$expectedType" with content for message key "$expectedMessageKey" """,
          s"""Response contains a submit button with id "$expectedElementId" and type "$expectedType" with content for message key "$expectedMessageKey" """
        )
      }
    }
  def containLink(expectedMessageKey: String, expectedHref: String, expectedClasses: Set[String] = Set.empty)(
    implicit messagesProvider: MessagesProvider): Matcher[Html] =
    new Matcher[Html] {
      override def apply(html: Html): MatchResult = {
        val doc = Jsoup.parse(html.toString)
        checkMessageIsDefined(expectedMessageKey)
        val foundElement = doc.select(s"a[href=$expectedHref]").first()

        val wasFoundWithCorrectMessage = Option(foundElement) match {
          case None          => false
          case Some(element) => element.text() == htmlEscapedMessage(expectedMessageKey)
        }

        val hasExpectedClasses = wasFoundWithCorrectMessage && expectedClasses.forall(foundElement.hasClass)

        MatchResult(
          wasFoundWithCorrectMessage && hasExpectedClasses,
          s"""Response does not contain a link to "$expectedHref" with content for message key "$expectedMessageKey" and with classes: "${expectedClasses
            .mkString(", ")}" """,
          s"""Response contains a link to "$expectedHref" with content for message key "$expectedMessageKey" and with classes: "${expectedClasses
            .mkString(", ")} """
        )
      }
    }

  def containLinkWithSubstring(expectedSubstring: String, expectedHref: String)(
    implicit messagesProvider: MessagesProvider): Matcher[Html] =
    new Matcher[Html] {
      override def apply(html: Html): MatchResult = {
        val doc = Jsoup.parse(html.toString)
        val foundElement = doc.select(s"a[href=$expectedHref]").first()
        val wasFoundWithCorrectMessage = Option(foundElement) match {
          case None          => false
          case Some(element) => element.text().contains(expectedSubstring)
        }
        MatchResult(
          wasFoundWithCorrectMessage,
          s"""Response does not contain a link to "$expectedHref" with content containing substring "$expectedSubstring" """,
          s"""Response contains a link to "$expectedHref" with content containing substring "$expectedSubstring" """
        )
      }
    }

  def repeatMessage(expectedMessageKey: String, times: Int)(
    implicit messagesProvider: MessagesProvider): Matcher[Html] = new Matcher[Html] {
    override def apply(html: Html): MatchResult =
      MatchResult(
        Messages(expectedMessageKey).r.findAllMatchIn(html.toString).size == times,
        s"The message keys $expectedMessageKey does not appear $times times in the content",
        s"The message keys $expectedMessageKey appears $times times in the content"
      )
  }
}
