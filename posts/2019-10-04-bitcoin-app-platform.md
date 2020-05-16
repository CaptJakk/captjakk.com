---
title: Bitcoin and the Rise of Sovereign Apps
---

Today I want to talk about how Bitcoin, in the coming years, can be used as more than just a currency. I have
recently started thinking about the Bitcoin network as a post-national court of disputes. I think this analogy
is helpful when trying to understand what can be done with Bitcoin, and the role the blockchain itself actually
plays in the future of Bitcoin.

## Languages and Contracts

It is a misconception that Bitcoin is not capable of smart contracts. This misunderstanding is primarily the result
of Ethereum's marketing as well as a very real criticism of the expressivity of Bitcoin's smart contract language.
Bitcoin's smart contracting language is very primitive today, and rather than deploying contracts as "agents" as
Ethereum treats them, Bitcoin's contracts are more like lockboxes with riddles that when answered correctly, the
coins may be moved. The difference between these models is that in Ethereum, contracts are used to execute specific
state transitions, whereas in Bitcoin contracts are used to block specific state transitions. Anyone who has ever
used Bitcoin has already experienced sending money to contracts. In fact, Bitcoin addresses basically encode two
standard contracts that are used: P2PKH, and P2SH

But there is merit to the criticism that Bitcoin's Script language is too limited. Today, when you write a Bitcoin
script, you may only ask for a few specific things: a delay effect (CLTV, CSV), a signature check (CheckSig/Verify,
CheckMultiSig/Verify), Hash Preimages (SHA256, RipeMD160), and some basic arithmetic operations (although
interestingly enough, not all of them). These are highly specific, and notably exclude asking for any information
about the outputs. In many cases this is fine, but if you enable output inspection, you get the ability to enforce
what we call "linearity" to your contract. Why is linearity important? Linearity gives us the ability to build
Finite State Machines whose transition rules are enforceable and verifiable. The state is carried in the UTXO set
and the state transition rules are encoded into the riddles on the output(s).

Why does this matter? It would allow things like escrows, many financial instruments, collateralized debt
instruments, credit reports/credit histories, all of which are important and beneficial to a robust Bitcoin
economy. Some Bitcoiners will try to deny the importance of these things, saying "Bitcoin doesn't need that", and
that may be true, it is entirely possible for Bitcoin to succeed without any of the bells and whistles. But I want
to change the attitude around that from "Does Bitcoin need it?" to "Is Bitcoin better with it?". I certainly agree
that sacrificing Bitcoin's security is not acceptable as a price to pay for increasing its functionality, but we
can absolutely add certain programming capabilities without these sacrifices.

So in order to give Bitcoin what it needs to be able to do arbitrarily powerful smart contracts is to have a
general way to talk about all parts of the transaction itself and to specify conditions on the relationships
between them. How general do we need? And how general is too general? It is the opinion of most Bitcoiners (myself
included) that Turing Complete is too general. Due to the halting problem, you cannot tell if a Turing Complete
program will ever terminate, nor bound how long it will take to compute. This opens you up to denial of service
attacks, and those cannot be mitigated without introducing a gas model like Ethereum. Ethereum's gas model also
has a reasonably terrible UX around it in my opinion, so that doesn't seem like the direction we'd want to go even
if we could. But the good news here is that we don't even need Turing Completeness to get a ton of useful
applications, and if we go with the slightly less powerful Finitary Completeness, we enable a huge number of
applications as well as retain the ability to statically analyze programs and bound their resource costs without a
gas model.

## Simplicity

This is where Simplicity comes in. Simplicity is a new programming "language" for Bitcoin. Calling it a language
is a bit generous as it is a tiny, tiny language. Tiny is good from the perspective of security and verifiability,
however, it does mean that it is so much more low level than anyone in the modern day would like to program by
hand. But a language it is, nonetheless. This language is finitarily complete, and has access as primitives to
all of the components of a Bitcoin transaction, including output inspection. As far as I know, it's the only
project that offers to give comprehensive access to the values of the transaction as well as a general way to
ensure that various conditions hold about those values.

But how do you change Bitcoin's programming language? Doesn't that change consensus? In the 2017 UASF, 3 really
important things happened to Bitcoin: #1 we fixed transaction malleability (the importance of this is well
documented and beyond the scope of this article), #2 is we separated soundness and Proof of Work validation from
witness (proof) validation (the namesake of the proposal: Segregated Witness), and the least commony talked about
#3, we added a script versioning system that allows new script types to be added via soft fork. This is perhaps
the least talked about effect of the SegWit changes, and is ultimately how the Taproot and Tapscript proposals
will get put into Bitcoin when we get around to that. It is also the method I would expect Simplicity to be added
to mainnet on Bitcoin. That said, grassroots support for adding Simplicity to Bitcoin is relatively small, which
is part of the motivation for this post, so it may take a while to get the social consensus needed for it to go
through.

Without diving too deep into why this is (although if you are interested, I recommend reading the source material),
the drawback of something like simplicity is that it can be very expensive to move simplicity transactions. As an
example, the most recent update I have is that the test transaction that requires a Schnorr Signature verification,
requires about 15Kb of witness data (signature). For reference there is only 3000Kb of witness space and the
witness data for the average transaction is 70-ish bytes. So this is about 200x more expensive than the average
transaction, but there are known ways to compress this substantially. But it gets better, because what if there was
a way to bypass having to submit the witness data altogether.

## Taproot

Earlier I mentioned Taproot. Taproot is probably the most important proposal happening in Bitcoin right now, and it
is primarily sold as a large improvement to privacy, but the way by which it goes about this massively improves a
number of other aspects we care about in Bitcoin, not the least of which is scalability.

There are two ways by which you can enhance privacy: Omission, and Obfuscation. Omission is about leaving as much
data out of the consensus checks as possible, and Obfuscation is about trying to submit extra data to the consensus
checks that hides which piece of data is actually the important part. The neat thing about omission is that it is
also a win for scaleability. Any data you can leave out and still preserve the integrity of your transaction is
blockspace that is saved. The key observation about Taproot is that the only reason any complicated smart contract
needs to be executed is because the people who are party to that contract are in dispute in some manner, and that
as long as they can cooperate, there is no reason the blockchain should have to settle the particulars of every
private arrangement. For this reason, taproot organizes every spend as being one of two flavors: either the
parties cooperate and you combine your signatures to spend the coins, or you dispute and you use the contract code
to settle the dispute. Finally, with Schnorr Signatures every multisig spend is indistinguisable from a single-sig
spend, masking all cooperative private contracts and making them look like individual transactions.

## My Lawyer is Advising Me to Settle

I want to briefly revisit my court system analogy. I'm from the US, and here were have reasonably strong rule of
law, and a court system that, by and large, people trust and respect. However, the court system doesn't have the
ability to hear every possible dispute that people have. So how do we have stable rule of law here? The answer lies
in the fact that laws are in most cases clearly interpretable by the would-be disputer, and the cost of
adjudicating a dispute is higher than cooperating. This pair of incentives causes people to choose to cooperate
as often as they possibly can, and as long as both parties can predict how the "judge" will rule, they both just
jump to the same conclusion and cooperate for the same result.

It is important to note here that there are still important considerations in designing the structure of your
contracts such that you avoid the "nothing at stake" problem, but the dynamics and game forces at play will
produce the cooperative result for rational actors as long as you do.

With those dynamics in mind, I want to go back and challenge the fact that a simplicity transaction is expensive.
The law is clear to all parties involved, it is expensive to publicly adjudicate, and so the parties will choose
to cooperate in pretty much any circumstance unless one of the actors is irrational, or there is nothing at stake
for that actor. Remember that all parties know what the ruling is going to be and the choice to not cooperate
comes at their own expense. By making the language that we write law in more general and expensive, we reduce the
number of cases that can be heard, but if people weren't taking to each other to court, then we essentially can
scale the complexity of our interactions with little to no realized cost. Additionally, we can keep private
interactions private in the cryptographic sense as well.

## You'll have to talk to my Agent

Cooperation comes at a slightly different cost, though. It requires any parties who are dealing with each other in
the above manner, to be running infrastructure that acts as their agent. People have used this as a criticism of
the lightning network, which is the first example of this that we've seen in the world. And while the BCash crowd
will lament over the idea that things aren't as they were in the "good old days" of layer 1 everything, people do
actually run this kind of infrastructure, which I think is a rather interesting development in the history of
personal computing.

People have come up with various solutions to this, and the market will decide how they want this problem to
ultimately get solved, but the infrastructure is useful for a whole set of applications that you might want to run
in a distributed fashion. When the default assumption is that everyone has personal, 99.9% live, addressable
infrastructure, there is a fundamentally new class of applications that can be built that was formerly untenable
from a UX perspective.

By packaging up these pieces of software in such a way that the average user can download and install new
applications to the aforementioned infrastructure, you enable what I have lately been calling "sovereign apps".

## Sovereign Apps

What are Sovereign Apps? I generally think of these as any software that cannot be detected and stopped at scale.
The first sovereign app that we ever had was email. It eventually got centralized by the vastly superior UX of
online services, but email was designed to be run in a distributed fashion. Another example is BitTorrent. And most
recently, Bitcoin. Between the immutability afforded to us by blockchains, and the censorship resistance properties
of peer to peer networks in general. You can pretty much do whatever you want in this world in the realm of
exchanging information and value. The introduction of being able to move value around like this supercharges the
sovereign apps that are primarily about information.

I'm reasonably certain that with this setup you can do all of the following without interference from outside
actors: Lightning Atomic Swaps, Lending (Collateralized and Unsecured), Credit Histories, Options, Distributed OTC
Exchanges, Wills, Trusts, Corporate Governance/Treasury Management, Ephemeral Twitter, Truly end to end encrypted
instant messaging, etc. If you think I'm wrong, please let me know (via Twitter or Email), as this is definitely
an ambitious list. But what I'm trying to say here is that Bitcoin and this new infrastructure is going to
catalyze the destruction of the monopoly service providers that we spent the last two decades building.

It remains to be seen where exactly the balance is with respect to which applications need to be sovereign and
which ones don't, and this could take a decade or two to play out, but this is going to be an important shift in
the capabilities of humanity, with a strong bias towards individual liberty, which I vehemently support.

With technologies like this, I think the future is very bright for the average person.