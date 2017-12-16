package mco.core

import com.olegpy.forwarders
import mco.core.paths._

import java.net.URL


/**
 * Algebra of operations related to storing images
 * Allows addition and removal of image to/from store
 * as well as limiting them to a given set of keys
 *
 * @tparam F context of result values
 */
@forwarders trait ImageStore[F[_]] {
  /**
   * Get an image associated with provided key
   * @param key the key image was previously associated with
   * @return Some(url of image) if association exists, None
   *         otherwise
   */
  def getImage(key: RelPath): F[Option[URL]]

  /**
   * Change association of given key to provided path
   * Consequent getImage is not guaranteed to return
   * the same path, but the contents at two should
   * be the same
   *
   * @param key the key to associate an image with
   * @param path Some(path) to create an association,
   *             None to remove it
   * @return Unit inside context F
   */
  def putImage(key: RelPath, path: Option[Path]): F[Unit]
//
//  /**
//   * Removes all images with keys not present inside a vector
//   * @param keys association keys which should be retained
//   * @return Unit inside context F
//   */
//  def stripImages(keys: Vector[RelPath]): F[Unit]
}
