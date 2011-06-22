package com.github.kputnam.fifth.types

import com.github.kputnam.fifth.misc.union._

case class WordType[+I: Union[UnknownType, StackType]#Value,
                    +O: Union[UnknownType, StackType]#Value](input: I, output: O) extends Type {
  override def toString =
    "(" + input + " -> " + output + ")"

  def hasOccurrence(t: UnknownType) =
    input.asInstanceOf[Type].hasOccurrence(t) || output.asInstanceOf[Type].hasOccurrence(t)

  def substitute(s: Substitution): Option[WordType[Type, Type]] = input match {
    case in: StackType => output match {
      case out: StackType =>
        in.substitute(s).flatMap(in => out.substitute(s).map(out => WordType(in, out)))
      case out: UnknownType => out.substitute(s).flatMap {
        case out: StackType =>
          in.substitute(s).map(in => WordType(in, out))
        case out: UnknownType =>
          in.substitute(s).map(in => WordType(in, out))
        case _ => None
      }
    }
    case in: UnknownType => in.substitute(s).flatMap {
      case in: StackType => output match {
        case out: StackType =>
          out.substitute(s).map(out => WordType(in, out))
        case out: UnknownType => out.substitute(s).flatMap {
          case out: StackType => Some(WordType(in, out))
          case out: UnknownType => Some(WordType(in, out))
          case _ => None
        }
      }
      case in: UnknownType => output match {
        case out: StackType =>
          out.substitute(s).map(out => WordType(in, out))
        case out: UnknownType => out.substitute(s) match {
          case out: StackType => Some(WordType(in, out))
          case out: UnknownType => Some(WordType(in, out))
          case _ => None
        }
      }
      case _ => None
    }
  }

  def unifyWith(t: Type, s: Substitution) = {
    substitute(s).flatMap { me =>
      t.substitute(s).flatMap {
        case he: UnknownType =>
          if (me.hasOccurrence(he)) None
          else s.addBinding(he, me)
        case he: WordType[Type, Type] =>
          me.input.unifyWith(he.input, s).flatMap(s => me.output.unifyWith(he.output, s))
        case _ => None
      }
    }
  }
}