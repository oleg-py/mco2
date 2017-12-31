package mco.io

import mco.core.paths.{Path, RelPath}


trait Archive[F[_]] {
  def entries: fs2.Stream[F, RelPath]
  def extract(rawTargets: Map[RelPath, Path]): F[Unit]
}