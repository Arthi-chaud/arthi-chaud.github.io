---
title: Self-hosting changed the way I listen to music
date: 2025-07-03
tags: [Meelo, Self-hosting]
description: A ROX on self-hosting media and how it impacted the way I consume multimedia
---

## Introduction

This blog post was inspired by a little incident that kept me from using my home server, which runs a couple of services, most for media streaming.
This made me realise the way I consume music when using a paid streaming service differs from when I use one of my self-hosted services. More specifically, I realised that I used paid streaming services and self-hosted ones for different purposes.

I wanted to share my reflection on streaming services, how it impacted the way I/we consume music, and explain why (in my humble opinion) you (yeah, _you_) should consider setting up a self-hosted service for streaming music[^e].

[^e]: by using legally obtained material, PLEASE! 🙏🏻 

To make things clear upfront, I do not think that using self-hosted services for media streaming is the only _good_ way to consume media. That would be hypocritical. Self-hosting is not accessible (financially speaking; plus, it requires a lot of technical skills) and requires maintenance effort. However, I do think that they have some advantages that streaming services don't. 

My perspective is from the music streaming perspective, but I am sure it can be applicable to other kinds of media, like movies.

## The issues with streaming services

Streaming services have two advantages: they are cheap and they are convenient.

They are cheap in the sense that if we compare the price of a monthly membership to the price of a single album (whether it's digital or physical), the membership will almost always be more attractive. This is great, because for the same price as one CD per month, we get access to this big catalogue with what seems to be every song ever released[^a].


![Price](/assets/img/self-hosted/price.png)
*Price of a CD Album at HMV vs. A monthly Spotify subscription*


Streaming services were probably a response to the growing popularity of illegal downloading (remember this [iconic ad](https://www.youtube.com/watch?v=_FtE0S-IyY0)?). By offering a cheap and practical solution, it makes the hassle of visiting suspicious/risky downloading sites and maintaining storage for the downloaded file kinda obsolete. This is good, because illegal downloading keeps the artist from getting money for their work[^a1].

With such a wide range of choice, users might face the paradox of choice: the more choices we have, the less easy it is to actually choose. Or worded differently: while the idea of having unlimited music at our fingertips is obnoxiously appealing, choosing what to play becomes an actual challenge, and a frustrating one at that.
As a 'solution', streaming services can use your listening history and compare it to other users' to recommend artists/work you might like. This is great because it can introduce you to new stuff, like artists that wouldn't have been in you radar or songs that you wouldn't think of checking out.

![Recommendations](/assets/img/self-hosted/recommendations.png)
*Radio-based recommendation feature form Apple Music*

However, you don't actually need a streaming service to get such recommendations. You can use a scrobbling service (which track your listen history) instead. However, the integration of such services with your streaming service may be challenged by how much information the later exposes through their API (for example, without surprises, Spotify provides more information than Apple Music).

I've noticed a pattern in the way people use streaming services to organise their music library, or should I say how people do _not_ actually organise their library. I see many users (myself included) just creating a playlist and putting songs they liked there. This can be explained by the fact that people don't really care about albums anymore[^b] and don't want to have to ask themselves 'what album does the song belong to?'.
Consequently, we end up with very large playlists that are hard to navigate, making the process of choosing a song to play a hassle.

![Throwaway playlist](/assets/img/self-hosted/throwaway-playlist.png){:w="300"}
*My 'throwaway' playlist*

One last thing I should point out is that the media available on streaming services can be removed or changed at any time (the most famous example is the 'Friends' series being removed from the Netflix catalogue). This means that, without any warnings, some _tracks_ can disappear/become unavailable. The _song_ might still be available, so it _might_ get replaced by a track from a totally unrelated album (usually a random compilation). This is particularly painful when you try to somewhat maintain your library on these streaming services: let's say you added multiple songs from a single album to your library, you don't have the guarantee that all these songs will still be there tomorrow. As someone who had used streaming services somewhat intensively for the past 5-6 years, this happened a lot, and it made me realise that 1) you can't really trust the service (and I am sure many people feel this way) and that 2) you don't actually _own_ the media, it can disappear anytime.


![Unavailable song](/assets/img/self-hosted/unavailable-song.png)
*Some songs become unavailable for no apparent reason (here, track 9)*

**TLDR**: While streaming services are convenient and cheap, they can be unreliable and kinda spoils the personal connection that we have with our music libraries.

[^a]: It's technically not the case, but this is what it _feels_ like.
[^a1]: Although it's very well-known by now that streaming service are not very lucrative for artist.
[^b]: This is not a throwaway statement, look it up. Artists/record labels prefer releasing songs almost regularly, instead of releasing them as a single body of work.

## Self-hosted music services: getting control back

Alternatively, one could self-host a service to stream content from a hard drive. As mentioned in the introduction, unfortunately, setting up and maintaining a home server is not something anyone can do, which is a shame. However, I think it's important to acknowledge that products like [Plex](https://www.plex.tv) and projects like [Jellyfin](https://github.com/jellyfin/jellyfin), [Swing](https://github.com/swingmx/swingmusic), [BlackCandy](https://github.com/blackcandy-org/blackcandy), etc. really helped democratise self-hosting, by having a very easy setup process, intuitive user interfaces and powerful metadata management systems.

![Plex](/assets/img/self-hosted/plex.png)
*Plex Preview*

Since such services rely on local files, it is not possible to have a catalogue as wide as Spotify's. Yes, you might not be able to discover new things as easily as with paid streaming services, but this is not what self-hosted services are for. Their goal is to make your files available to you, wherever you are.
Yes, you first need to get these files. And this is why self-hosted service can help you (re-)connect with media: *you* choose the content of your own catalogue. 

Whether you get your files from physical media (CDs, DVDs, etc.) or purchase them digitally, you had, at some point, decided to get these files, they were not served to you on a silver platter like streaming services do. By making the choice of getting these files, you are aware of them, and that will make it easier for your brain to make a choice when you are looking for something to listen to. (It's a bit like team building: teams will likely be more efficient when members know each other beforehand than if they don't). 

I mentioned earlier scrobbling services. The good news is that the self-hosted community really likes scrobbling. This means that most mature self-hosted music services will support integration with scrobblers ([LastFM](https://www.last.fm), [ListenBrainz](https://listenbrainz.org), etc.), allowing them to provide you relevant recommendations. 

## A solution: Using a bit of both

I have been using Apple Music for close to 5–6 years now. Four years ago I was introduced to the world of self-hosting, built my own home server and set up a Plex server. 

I use my paid subscription to browse the latest releases, scroll through the charts and let the recommendation system take me to new horizons.
On the other hand, when I know what kind of music I want to listen to, I'll intuitively go other my self-hosted service, because I know it'll be easier to find something that fits my mood. Because I am more familiar with my personal catalogue (or maybe because I have a lot of stuff that's not available on paid streaming services), I am more aware of what is available, and I don't scroll mindlessly and click randomly on a song to play.

I personally don't think that I could get rid of my streaming service subscription. However, I do believe that using both a subscription and a self-hosted service, using either one or the other according to what you need/how you want to experience music.

![Music Apps](/assets/img/self-hosted/use-both.png)


## Storytime

A month ago, I managed to brick my server remotely, losing access to my personal collection. However, I live abroad and my server being at a relative's, I couldn't fix it through the network[^d]. I had to wait three weeks before being able to get the server up and running again. During these three weeks, I was 'forced' to rely solely on my Apple Music subscription to listen to music. Even though I had access to this infinite catalogue, I felt weirdly restricted, overwhelmed by the amount of choice available. Consequently, I felt like I couldn't enjoy music because I was being overfed with so many choices and suggestions. It really felt like a breath of fresh air to be able to use my Plex server, just because it's only about me, myself and 'my' music.

[^d]: That was totally a skill issue on my end. I had to reinstall the entire OS X)

## Call-to-action

We're getting to the end of this blog post. If this resonated with you in any way, and if are able to self-host, I invite you to do the following:

- Select three to four albums that you like or mean something to you
  - Albums that listen to regularly, or that you haven't listened to in a long time.
  - If you can't find any, pick albums that contain one of your favourite songs
- Get them legally
  - Either play it old school and try to get them physically (it's fun, you get a nice booklet out of it, it's a nice memorabilia)
  - Or buy them digitally (iTunes Store, Amazon Music, etc.) (it's probably going to be cheaper)
- Set up a Plex or a Jellyfin server
  - I could recommend you [Meelo](http://github.com/Arthi-chaud/Meelo) *wink wink*, but the former solutions are easier to set up
  - Alternatively, if you are curious about what self-hosted services are out there, I recommend checking out '[Awesome-selfhosted](https://github.com/awesome-selfhosted/awesome-selfhosted?tab=readme-ov-file#media-streaming---audio-streaming)' or the [selfh.st](https://selfh.st/apps/?tag=Music) website.
- Give each album a full listen
  - If possible, play them front-to-back, without skipping songs
  - You don't need to focus at 100% on the music, feel free to do work, chores, workout…
  - Don't feel like you have to play all the albums you selected back to back.


What's the goal? By having a restricted selection and by mindfully choosing what you listen to, your brain will more easily connect with it, and you'll get more satisfaction out of it.



Thanks for reading! Now go enjoy your favourite tunes _mindfully_ and have a great day :)
