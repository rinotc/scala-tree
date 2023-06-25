package com.github.rinotc.tree

import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.diagrams.Diagrams
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

trait BaseTest extends AnyFunSpec with Diagrams with EitherValues with OptionValues with Matchers
