\docType{methods}
\name{build}
\alias{build}
\alias{build,character,ANY,ANY-method}
\alias{build,FigureTarget,ANY,missing-method}
\alias{build,FigureTarget,DirectoryLocation,Xdata-method}
\alias{build,FigureTarget,MemoryLocation,Xdata-method}
\alias{build,MdReport,Blogger,Xdata-method}
\alias{build,MdReport,DirectoryLocation,Xdata-method}
\alias{build,MdReport,MemoryLocation,Xdata-method}
\alias{build,PkgTarget,DirectoryLocation,missing-method}
\alias{build,PkgTarget,missing,missing-method}
\alias{build,SweaveReport,DirectoryLocation,ANY-method}
\alias{build,SweaveReport,missing,missing-method}
\alias{build,Target,character,missing-method}
\alias{build,Target,character,Xdata-method}
\alias{build,Target,Location,missing-method}
\alias{build,Target,missing,missing-method}
\alias{build,Target,missing,Xdata-method}
\alias{build,TextTarget,DirectoryLocation,Xdata-method}
\alias{build,TextTarget,MemoryLocation,Xdata-method}
\title{build a target}
\arguments{
  \item{target}{an object of class Target or derived}

  \item{where}{an object of class Location or derived}

  \item{xdata}{an object of class Xdata or derived}
}
\description{
  This generic builds a target. Currently, there are two
  targets defined: markdown reports and graphic files.
}

