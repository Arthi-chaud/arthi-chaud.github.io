---
title: Meelo - Visual Media as first-class citizens
date: 2025-02-14 
tags: [Meelo]
description:  Going through the implementation of support for Videos in Meelo
---

Today's post will focus on why and how Meelo supports Videos and Illustrations, *even though it's just a music server*. As for the previous post, this will not be a technical post, but more of an engineering one.

# Videos

## Music x Videos

Music videos have been a thing since the 60's. They help promote new material to the public. First, they would be broadcasted on television. Then, when VHS became a thing, people could buy them and watch their favourite videos whenever they wanted (then VHS got replaced by DVDs). In the 2000's, when YouTube wasn't a thing, CD Singles could be [*enhanced*](https://en.wikipedia.org/wiki/Enhanced_CD), i.e. have a data 'partition' with exclusive media like wallpapers, screensavers and music videos (mind you, the quality was terrible, with 15 FPS and dimensions close to 600x300). 
Then YouTube became popular, and the preferred distribution medium for music videos. In my opinion, it changed the way we consumed music, as music was freely and legaly accessible. Additionally, when one would play a song on YouTube, they would just play the music video. Thus, to people at the time, listening to music was tightly coupled with watching the music video.

Thus, in my opinion, Music Videos are an integral part of enjoying music. They bring a new (visual) dimension to what was previously an audio-only experience.

## Software and Music Videos

How do popular music library management software handle Music Videos?

- iTunes: Videos are regular tracks. There is no relation with songs and videos that share the same name.
  - They can be part of albums, and can have a dedicated thumbnail/illustration
  - Fun fact, if the last track of an album was a video, its thumbnail would become the album's artwork, ugh.
- Plex: According to the [documentation](https://support.plex.tv/articles/205568377-adding-local-artist-and-music-videos/), Music Videos must be in the same folder as the related song/track. This has a few shortcomings:
  - Videos cannot be tracks (unlike iTunes)
  - It's not possible to have albums only containing videos (so no support for video collection DVDs)
  - If the track's file is `1-02 My Track.m4a`, the video should be named `1-02 My Track.mp4`. I'll be honest, I don't like it
  - An alternative approach is to have a secondary 'Other Video' library, but no metadata will be processed.
- Jellyfin supports Music Videos through a [plugin](https://github.com/jellyfin/jellyfin-plugin-imvdb), but I have not tested it, and it looks like it does not link videos with music media.

We can conclude that Music Videos are not really thought about for these software. Well, I guess Meelo can fix this!

## Meelo's support for Music Videos

Pre-v3.0, Music Videos where only modelled as tracks with a 'Video' type. Simple, easy, done, right?

Well, this approach has a few limitations:

- What about videos that are not music videos? (e.g. album trailers, behind-the-scenes) There would be 'song' entries created even though it's not musical content.
- There is no way to differentiate music videos for a same song

v3.0 put Music Videos (or video content) to the foreground and also supports 'standalone tracks', meaning that:

- Videos do not have to belong to an album
  - Useful when you have videos bought from the iTunes Stores or *obtained* from YouTube.
- There is a new `Video` model, with a one-to-many relation with the `Track` model. 
  - `Track` can be related to at least a `Song` or a `Video`.
  - `Video` has an optional relation to `Song` 
    - e.g. you don't want non-music videos to be considered as a song
  - `Song` has a one-to-many relation to `Video`
    - So songs can have multiple music videos
- Just like albums and songs, videos have a *type*
  - Music Video, Lyric Video, Behind-the-scenes, Interview, etc.

## Illustrations

To me, illustrations (artworks, thumbnails, etc.) should be central to the engineering of a music server. Playing music without seeing the album artwork is like playing file from a file browser: a bare-bones, forgettable experience.

Since Meelo sees Albums as groups of releases, it does not handle illustrations as naively as other music servers would do.
Additionally, Meelo handles disc- and track-specific illustrations, meaning that:

- There is base `Illustration` model, with a [blurhash](https://blurha.sh), an aspect-ration etc.
  - Artists use this model directly
- There is a `ReleaseIllustration` model with:
  - An optional disc and track index
  - A one-to-one relation with the `Illustration` model
  - A many-to-one relation with the `Release` model
- Release's illustration will be the `ReleaseIllustration` with a disc index set to null or the lowest disc index and the track index set to null
- Album's illustration is the master release's illustration
- If tracks are 'standalone' (i.e. do not belong to a release), they have an `Illustration`
- Song's illustration is the master track's illustration.

Why should illustration be disc-specific? It all comes down to an edge-case. 
In the UK (mainly), in the 2000's, CD Singles were sold in two parts (labelled 'CD1', 'CD2') (here is an example [CD1](https://www.discogs.com/release/1794920-Sugababes-Stronger-Angels-With-Dirty-Faces) [CD2](https://www.discogs.com/release/1292190-Sugababes-Stronger-Angels-With-Dirty-Faces)).
Usually, these CDs had totally different artworks. And when you structure your files, you would usually put CD1 and CD2 into a single (no pun intended) album, with 2 disc. Thus, the need for disc-specific illustration (which iTunes does well).


