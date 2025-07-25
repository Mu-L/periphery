import Foundation

public final class Declaration {
    public enum Kind: String, RawRepresentable, CaseIterable {
        case `associatedtype`
        case `class`
        case `enum`
        case enumelement
        case `extension`
        case extensionClass = "extension.class"
        case extensionEnum = "extension.enum"
        case extensionProtocol = "extension.protocol"
        case extensionStruct = "extension.struct"
        case functionAccessorAddress = "function.accessor.address"
        case functionAccessorDidset = "function.accessor.didset"
        case functionAccessorGetter = "function.accessor.getter"
        case functionAccessorMutableaddress = "function.accessor.mutableaddress"
        case functionAccessorSetter = "function.accessor.setter"
        case functionAccessorWillset = "function.accessor.willset"
        case functionAccessorRead = "function.accessor.read"
        case functionAccessorModify = "function.accessor.modify"
        case functionAccessorInit = "function.accessor.init"
        case functionConstructor = "function.constructor"
        case functionDestructor = "function.destructor"
        case functionFree = "function.free"
        case functionMethodClass = "function.method.class"
        case functionMethodInstance = "function.method.instance"
        case functionMethodStatic = "function.method.static"
        case functionOperator = "function.operator"
        case functionOperatorInfix = "function.operator.infix"
        case functionOperatorPostfix = "function.operator.postfix"
        case functionOperatorPrefix = "function.operator.prefix"
        case functionSubscript = "function.subscript"
        case genericTypeParam = "generic_type_param"
        case module
        case precedenceGroup = "precedencegroup"
        case `protocol`
        case `struct`
        case `typealias`
        case varClass = "var.class"
        case varGlobal = "var.global"
        case varInstance = "var.instance"
        case varLocal = "var.local"
        case varParameter = "var.parameter"
        case varStatic = "var.static"
        case macro

        static var functionKinds: Set<Kind> {
            Set(Kind.allCases.filter(\.isFunctionKind))
        }

        static var variableKinds: Set<Kind> {
            Set(Kind.allCases.filter(\.isVariableKind))
        }

        static var protocolMemberKinds: [Kind] {
            let functionKinds: [Kind] = [.functionMethodInstance, .functionMethodStatic, .functionSubscript, .functionOperator, .functionOperatorInfix, .functionOperatorPostfix, .functionOperatorPrefix, .functionConstructor]
            let variableKinds: [Kind] = [.varInstance, .varStatic]
            return functionKinds + variableKinds
        }

        static var protocolMemberConformingKinds: [Kind] {
            // Protocols cannot declare 'class' members, yet classes can fulfill the requirement with either a 'class'
            // or 'static' member.
            protocolMemberKinds + [.varClass, .functionMethodClass, .associatedtype]
        }

        public var isProtocolMemberKind: Bool {
            Self.protocolMemberKinds.contains(self)
        }

        public var isProtocolMemberConformingKind: Bool {
            Self.protocolMemberConformingKinds.contains(self)
        }

        public var isFunctionKind: Bool {
            rawValue.hasPrefix("function")
        }

        public var isVariableKind: Bool {
            rawValue.hasPrefix("var")
        }

        static var globalKinds: Set<Kind> = [
            .class,
            .protocol,
            .enum,
            .struct,
            .typealias,
            .functionFree,
            .extensionClass,
            .extensionStruct,
            .extensionProtocol,
            .varGlobal,
        ]

        static var extensionKinds: Set<Kind> {
            Set(Kind.allCases.filter(\.isExtensionKind))
        }

        public var extendedKind: Kind? {
            switch self {
            case .extensionClass:
                .class
            case .extensionStruct:
                .struct
            case .extensionEnum:
                .enum
            case .extensionProtocol:
                .protocol
            default:
                nil
            }
        }

        public var extensionKind: Kind? {
            switch self {
            case .class:
                .extensionClass
            case .struct:
                .extensionStruct
            case .enum:
                .extensionEnum
            case .protocol:
                .extensionProtocol
            default:
                nil
            }
        }

        public var isExtensionKind: Bool {
            rawValue.hasPrefix("extension")
        }

        public var isExtendableKind: Bool {
            isConcreteTypeDeclarableKind || self == .protocol
        }

        public var isConformableKind: Bool {
            isDiscreteConformableKind || isExtensionKind
        }

        public var isDiscreteConformableKind: Bool {
            Self.discreteConformableKinds.contains(self)
        }

        static var discreteConformableKinds: Set<Kind> {
            [.class, .struct, .enum]
        }

        public var isConcreteTypeDeclarableKind: Bool {
            Self.concreteTypeDeclarableKinds.contains(self)
        }

        static var concreteTypeDeclarableKinds: Set<Kind> {
            [.class, .struct, .enum, .typealias]
        }

        static var accessorKinds: Set<Kind> {
            Set(Kind.allCases.filter(\.isAccessorKind))
        }

        static var accessibleKinds: Set<Kind> {
            functionKinds.union(variableKinds).union(globalKinds)
        }

        static var overrideKinds: Set<Kind> {
            [.functionMethodInstance, .varInstance]
        }

        public var isAccessorKind: Bool {
            rawValue.hasPrefix("function.accessor")
        }

        static var toplevelAttributableKind: Set<Kind> {
            [.class, .struct, .enum]
        }

        public var displayName: String {
            switch self {
            case .module:
                "imported module"
            case .class:
                "class"
            case .protocol:
                "protocol"
            case .struct:
                "struct"
            case .enum:
                "enum"
            case .enumelement:
                "enum case"
            case .typealias:
                "typealias"
            case .associatedtype:
                "associatedtype"
            case .functionConstructor:
                "initializer"
            case .extension, .extensionEnum, .extensionClass, .extensionStruct, .extensionProtocol:
                "extension"
            case .functionMethodClass, .functionMethodStatic, .functionMethodInstance, .functionFree, .functionOperator, .functionOperatorInfix, .functionOperatorPostfix, .functionOperatorPrefix, .functionSubscript, .functionAccessorAddress, .functionAccessorMutableaddress, .functionAccessorDidset, .functionAccessorGetter, .functionAccessorSetter, .functionAccessorWillset, .functionAccessorRead, .functionAccessorModify, .functionAccessorInit, .functionDestructor:
                "function"
            case .varStatic, .varInstance, .varClass, .varGlobal, .varLocal:
                "property"
            case .varParameter:
                "parameter"
            case .genericTypeParam:
                "generic type parameter"
            case .precedenceGroup:
                "precedence group"
            case .macro:
                "macro"
            }
        }
    }

    public let location: Location
    public var attributes: Set<String> = []
    public var modifiers: Set<String> = []
    public var accessibility: DeclarationAccessibility = .init(value: .internal, isExplicit: false)
    public let kind: Kind
    public var name: String?
    public let usrs: Set<String>
    public var unusedParameters: Set<Declaration> = []
    public var declarations: Set<Declaration> = []
    public var commentCommands: Set<CommentCommand> = []
    public var references: Set<Reference> = []
    public var declaredType: String?
    public var hasGenericFunctionReturnedMetatypeParameters: Bool = false
    public var parent: Declaration?
    public var related: Set<Reference> = []
    public var isImplicit: Bool = false
    public var isObjcAccessible: Bool = false

    private let hashValueCache: Int

    public var ancestralDeclarations: Set<Declaration> {
        var maybeParent = parent
        var declarations: Set<Declaration> = []

        while let thisParent = maybeParent {
            declarations.insert(thisParent)
            maybeParent = thisParent.parent
        }

        return declarations
    }

    public var descendentDeclarations: Set<Declaration> {
        declarations
            .flatMapSet { $0.descendentDeclarations }
            .union(declarations)
            .union(unusedParameters)
    }

    public var immediateInheritedTypeReferences: Set<Reference> {
        let superclassReferences = related.filter { [.class, .struct, .protocol].contains($0.kind) }

        // Inherited typealiases are References instead of a Related.
        let typealiasReferences = references.filter { $0.kind == .typealias }
        return superclassReferences.union(typealiasReferences)
    }

    public var isComplexProperty: Bool {
        declarations.contains {
            if [
                .functionAccessorWillset,
                .functionAccessorDidset,
            ].contains($0.kind) {
                return true
            }

            if $0.kind.isAccessorKind, !$0.references.isEmpty {
                return true
            }

            return false
        }
    }

    public var isOverride: Bool {
        modifiers.contains("override")
    }

    public var relatedEquivalentReferences: [Reference] {
        related.filter { $0.kind == kind && $0.name == name }
    }

    public init(kind: Kind, usrs: Set<String>, location: Location) {
        self.kind = kind
        self.usrs = usrs
        self.location = location
        hashValueCache = usrs.hashValue
    }

    func isDeclaredInExtension(kind: Declaration.Kind) -> Bool {
        guard let parent else { return false }
        return parent.kind == kind
    }
}

extension Declaration: Hashable {
    public func hash(into hasher: inout Hasher) {
        hasher.combine(hashValueCache)
    }
}

extension Declaration: Equatable {
    public static func == (lhs: Declaration, rhs: Declaration) -> Bool {
        lhs.usrs == rhs.usrs
    }
}

extension Declaration: CustomStringConvertible {
    public var description: String {
        "Declaration(\(descriptionParts.joined(separator: ", ")))"
    }

    private var descriptionParts: [String] {
        let formattedName = name != nil ? "'\(name!)'" : "nil"
        let formattedAttributes = "[" + attributes.sorted().joined(separator: ", ") + "]"
        let formattedModifiers = "[" + modifiers.sorted().joined(separator: ", ") + "]"
        let formattedCommentCommands = "[" + commentCommands.map(\.description).sorted().joined(separator: ", ") + "]"
        let formattedUsrs = "[" + usrs.sorted().joined(separator: ", ") + "]"
        let implicitOrExplicit = isImplicit ? "implicit" : "explicit"
        return [
            kind.rawValue,
            formattedName,
            implicitOrExplicit,
            accessibility.value.rawValue,
            formattedModifiers,
            formattedAttributes,
            formattedCommentCommands,
            formattedUsrs,
            location.shortDescription,
        ]
    }
}

extension Declaration: Comparable {
    public static func < (lhs: Declaration, rhs: Declaration) -> Bool {
        if lhs.location == rhs.location {
            return lhs.usrs.sorted().joined() < rhs.usrs.sorted().joined()
        }

        return lhs.location < rhs.location
    }
}

public struct DeclarationAccessibility {
    public let value: Accessibility
    public let isExplicit: Bool

    public init(value: Accessibility, isExplicit: Bool) {
        self.value = value
        self.isExplicit = isExplicit
    }

    func isExplicitly(_ testValue: Accessibility) -> Bool {
        isExplicit && value == testValue
    }

    var isAccessibleCrossModule: Bool {
        value == .public || value == .open
    }
}
