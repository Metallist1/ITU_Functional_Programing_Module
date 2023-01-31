/********************************
 * Final Exam: Advanced Programming, by Andrzej Wąsowski
 * IT University of Copenhagen, Autumn 2022: 04 January 2023
 *
 * The exam consists of 11 questions to be solved within 4 hours.
 *
 * You can use any function from the course (textbook, exercises) in the
 * solutions, as well as any standard library functions. You can access any
 * static written material, also online, but you are not allowed to communicate
 * with anybody or with anything (bots). Using GitHub copilot during exam is
 * not allowed. By submitting you legally declare to have solved the problems
 * alone, without communicating with anybody.
 *
 * Solve the tasks in the file 'Exam.scala' (this file) found in zip archive
 * made available on LearnIt.
 *
 * Submit this file and only this file to learnIT. Do not convert to any other
 * format than .scala. Do not submit the entire zip archive. Do not reorder the
 * answers, and do not remove question numbers from the file. The only accepted
 * file format is '.scala'.
 *
 * Keep the solutions within 80 columns width to make grading easier.
 *
 * The answers will be graded manually. We focus on the correctness of ideas,
 * the use of concepts, clarity, and style. We are permissive on minor issues
 * such as semicolons, commas, other punctuation, small deviations in function
 * names, switching between curried and not curried arguments, etc. We will not
 * check whether the type inference succeeds. It suffices that a human reader
 * could infer types.
 *
 * We do not recommend solving questions to the point when they compile and
 * pass tests. Dependency problems and other technical issues can take a lot of
 * time, so only do this, once you are done with drafting all answers. If you
 * do compile, the files are set up for scala-cli 0.1.18 with the necessary
 * library dependencies configured, and the source files from the semester are
 * included.
 *
 * The percentage included at the beginning of each question will be used as a
 * soft indicative weight in grading.
 *
 * Good luck!
 */
package adpro

import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary.*, Prop.*
import org.scalactic.Equality

import fpinscala.answers.laziness.LazyList
import fpinscala.answers.state.*
import fpinscala.answers.monoids.Foldable
import fpinscala.answers.parallelism.Par
import fpinscala.answers.monads.Monad
